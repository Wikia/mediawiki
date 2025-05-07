<?php
/**
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 * http://www.gnu.org/copyleft/gpl.html
 *
 * @file
 */

namespace MediaWiki\User;

use InvalidArgumentException;
use LogicException;
use MediaWiki\Config\ServiceOptions;
use MediaWiki\HookContainer\HookContainer;
use MediaWiki\HookContainer\HookRunner;
use MediaWiki\Logging\ManualLogEntry;
use MediaWiki\MainConfigNames;
use MediaWiki\Parser\Sanitizer;
use MediaWiki\Permissions\Authority;
use MediaWiki\Permissions\GroupPermissionsLookup;
use MediaWiki\User\TempUser\TempUserConfig;
use MediaWiki\WikiMap\WikiMap;
use Psr\Log\LoggerInterface;
use Wikimedia\Assert\Assert;
use Wikimedia\IPUtils;
use Wikimedia\Rdbms\IDBAccessObject;
use Wikimedia\Rdbms\ReadOnlyMode;

/**
 * Manage user group memberships.
 *
 * @since 1.35
 * @ingroup User
 */
class UserGroupManager {

	/**
	 * @internal For use by ServiceWiring
	 */
	public const CONSTRUCTOR_OPTIONS = [
		MainConfigNames::AddGroups,
		MainConfigNames::AutoConfirmAge,
		MainConfigNames::AutoConfirmCount,
		MainConfigNames::Autopromote,
		MainConfigNames::AutopromoteOnce,
		MainConfigNames::AutopromoteOnceLogInRC,
		MainConfigNames::AutopromoteOnceRCExcludedGroups,
		MainConfigNames::EmailAuthentication,
		MainConfigNames::ImplicitGroups,
		MainConfigNames::GroupInheritsPermissions,
		MainConfigNames::GroupPermissions,
		MainConfigNames::GroupsAddToSelf,
		MainConfigNames::GroupsRemoveFromSelf,
		MainConfigNames::RevokePermissions,
		MainConfigNames::RemoveGroups,
		MainConfigNames::PrivilegedGroups,
	];

	/**
	 * Logical operators recognized in $wgAutopromote.
	 *
	 * @since 1.42
	 */
	public const VALID_OPS = [ '&', '|', '^', '!' ];

	private ServiceOptions $options;
	private HookContainer $hookContainer;
	private HookRunner $hookRunner;
	private ReadOnlyMode $readOnlyMode;
	private UserEditTracker $userEditTracker;
	private GroupPermissionsLookup $groupPermissionsLookup;
	private LoggerInterface $logger;
	private TempUserConfig $tempUserConfig;

	/** @var callable[] */
	private $clearCacheCallbacks;

	/** @var string|false */
	private $wikiId;

	/** string key for implicit groups cache */
	private const CACHE_IMPLICIT = 'implicit';

	/** string key for effective groups cache */
	private const CACHE_EFFECTIVE = 'effective';

	/** string key for group memberships cache */
	private const CACHE_MEMBERSHIP = 'membership';

	/** string key for former groups cache */
	private const CACHE_FORMER = 'former';

	/** string key for former groups cache */
	private const CACHE_PRIVILEGED = 'privileged';

	/**
	 * @var array Service caches, an assoc. array keyed after the user-keys generated
	 * by the getCacheKey method and storing values in the following format:
	 *
	 * userKey => [
	 *   self::CACHE_IMPLICIT => implicit groups cache
	 *   self::CACHE_EFFECTIVE => effective groups cache
	 *   self::CACHE_MEMBERSHIP => [ ] // Array of UserGroupMembership objects
	 *   self::CACHE_FORMER => former groups cache
	 *   self::CACHE_PRIVILEGED => privileged groups cache
	 * ]
	 */
	private $userGroupCache = [];

	/**
	 * @var array An assoc. array that stores query flags used to retrieve user groups
	 * from the database and is stored in the following format:
	 *
	 * userKey => [
	 *   self::CACHE_IMPLICIT => implicit groups query flag
	 *   self::CACHE_EFFECTIVE => effective groups query flag
	 *   self::CACHE_MEMBERSHIP => membership groups query flag
	 *   self::CACHE_FORMER => former groups query flag
	 *   self::CACHE_PRIVILEGED => privileged groups query flag
	 * ]
	 */
	private $queryFlagsUsedForCaching = [];

	/**
	 * @internal For use preventing an infinite loop when checking APCOND_BLOCKED
	 * @var array An assoc. array mapping the getCacheKey userKey to a bool indicating
	 * an ongoing condition check.
	 */
	private $recursionMap = [];
	private UserGroupStore $store;

	/**
	 * @param ServiceOptions $options
	 * @param ReadOnlyMode $readOnlyMode
	 * @param UserGroupStore $userGroupStore
	 * @param HookContainer $hookContainer
	 * @param UserEditTracker $userEditTracker
	 * @param GroupPermissionsLookup $groupPermissionsLookup
	 * @param LoggerInterface $logger
	 * @param TempUserConfig $tempUserConfig
	 * @param callable[] $clearCacheCallbacks
	 * @param string|false $wikiId
	 */
	public function __construct(
		ServiceOptions $options,
		ReadOnlyMode $readOnlyMode,
		UserGroupStore $userGroupStore,
		HookContainer $hookContainer,
		UserEditTracker $userEditTracker,
		GroupPermissionsLookup $groupPermissionsLookup,
		LoggerInterface $logger,
		TempUserConfig $tempUserConfig,
		array $clearCacheCallbacks = [],
		$wikiId = UserIdentity::LOCAL
	) {
		$options->assertRequiredOptions( self::CONSTRUCTOR_OPTIONS );
		$this->options = $options;
		$this->hookContainer = $hookContainer;
		$this->hookRunner = new HookRunner( $hookContainer );
		$this->userEditTracker = $userEditTracker;
		$this->groupPermissionsLookup = $groupPermissionsLookup;
		$this->logger = $logger;
		$this->tempUserConfig = $tempUserConfig;
		$this->readOnlyMode = $readOnlyMode;
		$this->clearCacheCallbacks = $clearCacheCallbacks;
		$this->wikiId = $wikiId;
		$this->store = $userGroupStore;
	}

	/**
	 * Return the set of defined explicit groups.
	 * The implicit groups (by default *, 'user' and 'autoconfirmed')
	 * are not included, as they are defined automatically, not in the database.
	 * @return string[] internal group names
	 */
	public function listAllGroups(): array {
		return array_values( array_unique(
			array_diff(
				array_merge(
					array_keys( $this->options->get( MainConfigNames::GroupPermissions ) ),
					array_keys( $this->options->get( MainConfigNames::RevokePermissions ) ),
					array_keys( $this->options->get( MainConfigNames::GroupInheritsPermissions ) )
				),
				$this->listAllImplicitGroups()
			)
		) );
	}

	/**
	 * Get a list of all configured implicit groups
	 * @return string[]
	 */
	public function listAllImplicitGroups(): array {
		return $this->options->get( MainConfigNames::ImplicitGroups );
	}

	/**
	 * Creates a new UserGroupMembership instance from $row.
	 * The fields required to build an instance could be
	 * found using getQueryInfo() method.
	 *
	 * @param \stdClass $row A database result object
	 *
	 * @return UserGroupMembership
	 */
	public function newGroupMembershipFromRow( \stdClass $row ): UserGroupMembership {
		return $this->store->newGroupMembershipFromRow( $row );
	}

	/**
	 * Load the user groups cache from the provided user groups data
	 * @internal for use by the User object only
	 * @param UserIdentity $user
	 * @param array $userGroups an array of database query results
	 * @param int $queryFlags
	 */
	public function loadGroupMembershipsFromArray(
		UserIdentity $user,
		array $userGroups,
		int $queryFlags = IDBAccessObject::READ_NORMAL
	) {
		$user->assertWiki( $this->wikiId );
		$membershipGroups = [];
		reset( $userGroups );
		foreach ( $userGroups as $row ) {
			$ugm = $this->newGroupMembershipFromRow( $row );
			$membershipGroups[ $ugm->getGroup() ] = $ugm;
		}
		$this->setCache(
			$this->getCacheKey( $user ),
			self::CACHE_MEMBERSHIP,
			$membershipGroups,
			$queryFlags
		);
	}

	/**
	 * Get the list of implicit group memberships this user has.
	 *
	 * This includes 'user' if logged in, '*' for all accounts,
	 * and autopromoted groups
	 *
	 * @param UserIdentity $user
	 * @param int $queryFlags
	 * @param bool $recache Whether to avoid the cache
	 * @return string[] internal group names
	 */
	public function getUserImplicitGroups(
		UserIdentity $user,
		int $queryFlags = IDBAccessObject::READ_NORMAL,
		bool $recache = false
	): array {
		$user->assertWiki( $this->wikiId );
		$userKey = $this->getCacheKey( $user );
		if ( $recache ||
			!isset( $this->userGroupCache[$userKey][self::CACHE_IMPLICIT] ) ||
			!$this->canUseCachedValues( $user, self::CACHE_IMPLICIT, $queryFlags )
		) {
			$groups = [ '*' ];
			if ( $this->tempUserConfig->isTempName( $user->getName() ) ) {
				$groups[] = 'temp';
			} elseif ( $user->isRegistered() ) {
				$groups[] = 'user';
				$groups = array_unique( array_merge(
					$groups,
					$this->getUserAutopromoteGroups( $user )
				) );
			}
			$this->setCache( $userKey, self::CACHE_IMPLICIT, $groups, $queryFlags );
			if ( $recache ) {
				// Assure data consistency with rights/groups,
				// as getUserEffectiveGroups() depends on this function
				$this->clearUserCacheForKind( $user, self::CACHE_EFFECTIVE );
			}
		}
		return $this->userGroupCache[$userKey][self::CACHE_IMPLICIT];
	}

	/**
	 * Get the list of implicit group memberships the user has.
	 *
	 * This includes all explicit groups, plus 'user' if logged in,
	 * '*' for all accounts, and autopromoted groups
	 *
	 * @param UserIdentity $user
	 * @param int $queryFlags
	 * @param bool $recache Whether to avoid the cache
	 * @return string[] internal group names
	 */
	public function getUserEffectiveGroups(
		UserIdentity $user,
		int $queryFlags = IDBAccessObject::READ_NORMAL,
		bool $recache = false
	): array {
		$user->assertWiki( $this->wikiId );
		$userKey = $this->getCacheKey( $user );
		// Ignore cache if the $recache flag is set, cached values can not be used
		// or the cache value is missing
		if ( $recache ||
			!$this->canUseCachedValues( $user, self::CACHE_EFFECTIVE, $queryFlags ) ||
			!isset( $this->userGroupCache[$userKey][self::CACHE_EFFECTIVE] )
		) {
			$groups = array_unique( array_merge(
				$this->getUserGroups( $user, $queryFlags ), // explicit groups
				$this->getUserImplicitGroups( $user, $queryFlags, $recache ) // implicit groups
			) );
			// TODO: Deprecate passing out user object in the hook by introducing
			// an alternative hook
			if ( $this->hookContainer->isRegistered( 'UserEffectiveGroups' ) ) {
				$userObj = User::newFromIdentity( $user );
				$userObj->load();
				// Hook for additional groups
				$this->hookRunner->onUserEffectiveGroups( $userObj, $groups );
			}
			// Force reindexation of groups when a hook has unset one of them
			$effectiveGroups = array_values( array_unique( $groups ) );
			$this->setCache( $userKey, self::CACHE_EFFECTIVE, $effectiveGroups, $queryFlags );
		}
		return $this->userGroupCache[$userKey][self::CACHE_EFFECTIVE];
	}

	/**
	 * Returns the groups the user has belonged to.
	 *
	 * The user may still belong to the returned groups. Compare with
	 * getUserGroups().
	 *
	 * The function will not return groups the user had belonged to before MW 1.17
	 *
	 * @param UserIdentity $user
	 * @param int $queryFlags
	 * @return string[] Names of the groups the user has belonged to.
	 */
	public function getUserFormerGroups(
		UserIdentity $user,
		int $queryFlags = IDBAccessObject::READ_NORMAL
	): array {
		$user->assertWiki( $this->wikiId );
		$userKey = $this->getCacheKey( $user );

		if ( $this->canUseCachedValues( $user, self::CACHE_FORMER, $queryFlags ) &&
			isset( $this->userGroupCache[$userKey][self::CACHE_FORMER] )
		) {
			return $this->userGroupCache[$userKey][self::CACHE_FORMER];
		}

		if ( !$user->isRegistered() ) {
			// Anon users don't have groups stored in the database
			return [];
		}

		$formerGroups = $this->store->getFormerGroups( $user, $queryFlags );
		$this->setCache( $userKey, self::CACHE_FORMER, $formerGroups, $queryFlags );

		return $this->userGroupCache[$userKey][self::CACHE_FORMER];
	}

	/**
	 * Get the groups for the given user based on $wgAutopromote.
	 *
	 * @param UserIdentity $user The user to get the groups for
	 * @return string[] Array of groups to promote to.
	 *
	 * @see $wgAutopromote
	 */
	public function getUserAutopromoteGroups( UserIdentity $user ): array {
		$user->assertWiki( $this->wikiId );
		$promote = [];
		// TODO: remove the need for the full user object
		$userObj = User::newFromIdentity( $user );
		if ( $userObj->isTemp() ) {
			return [];
		}
		foreach ( $this->options->get( MainConfigNames::Autopromote ) as $group => $cond ) {
			if ( $this->recCheckCondition( $cond, $userObj ) ) {
				$promote[] = $group;
			}
		}

		$this->hookRunner->onGetAutoPromoteGroups( $userObj, $promote );
		return $promote;
	}

	/**
	 * Get the groups for the given user based on the given criteria.
	 *
	 * Does not return groups the user already belongs to or has once belonged.
	 *
	 * @param UserIdentity $user The user to get the groups for
	 * @param string $event Key in $wgAutopromoteOnce (each event has groups/criteria)
	 *
	 * @return string[] Groups the user should be promoted to.
	 *
	 * @see $wgAutopromoteOnce
	 */
	public function getUserAutopromoteOnceGroups(
		UserIdentity $user,
		string $event
	): array {
		$user->assertWiki( $this->wikiId );
		$autopromoteOnce = $this->options->get( MainConfigNames::AutopromoteOnce );
		$promote = [];

		if ( isset( $autopromoteOnce[$event] ) && count( $autopromoteOnce[$event] ) ) {
			// TODO: remove the need for the full user object
			$userObj = User::newFromIdentity( $user );
			if ( $userObj->isTemp() ) {
				return [];
			}
			$currentGroups = $this->getUserGroups( $user );
			$formerGroups = $this->getUserFormerGroups( $user );
			foreach ( $autopromoteOnce[$event] as $group => $cond ) {
				// Do not check if the user's already a member
				if ( in_array( $group, $currentGroups ) ) {
					continue;
				}
				// Do not autopromote if the user has belonged to the group
				if ( in_array( $group, $formerGroups ) ) {
					continue;
				}
				// Finally - check the conditions
				if ( $this->recCheckCondition( $cond, $userObj ) ) {
					$promote[] = $group;
				}
			}
		}

		return $promote;
	}

	/**
	 * Returns the list of privileged groups that $user belongs to.
	 * Privileged groups are ones that can be abused in a dangerous way.
	 *
	 * Depending on how extensions extend this method, it might return values
	 * that are not strictly user groups (ACL list names, etc.).
	 * It is meant for logging/auditing, not for passing to methods that expect group names.
	 *
	 * @param UserIdentity $user
	 * @param int $queryFlags
	 * @param bool $recache Whether to avoid the cache
	 * @return string[]
	 * @since 1.41 (also backported to 1.39.5 and 1.40.1)
	 * @see $wgPrivilegedGroups
	 * @see https://www.mediawiki.org/wiki/Manual:Hooks/UserGetPrivilegedGroups
	 */
	public function getUserPrivilegedGroups(
		UserIdentity $user,
		int $queryFlags = IDBAccessObject::READ_NORMAL,
		bool $recache = false
	): array {
		$userKey = $this->getCacheKey( $user );

		if ( !$recache &&
			$this->canUseCachedValues( $user, self::CACHE_PRIVILEGED, $queryFlags ) &&
			isset( $this->userGroupCache[$userKey][self::CACHE_PRIVILEGED] )
		) {
			return $this->userGroupCache[$userKey][self::CACHE_PRIVILEGED];
		}

		if ( !$user->isRegistered() ) {
			return [];
		}

		$groups = array_intersect(
			$this->getUserEffectiveGroups( $user, $queryFlags, $recache ),
			$this->options->get( 'PrivilegedGroups' )
		);

		$this->hookRunner->onUserPrivilegedGroups( $user, $groups );

		$this->setCache(
			$this->getCacheKey( $user ),
			self::CACHE_PRIVILEGED,
			array_values( array_unique( $groups ) ),
			$queryFlags
		);

		return $this->userGroupCache[$userKey][self::CACHE_PRIVILEGED];
	}

	/**
	 * Recursively check a condition.  Conditions are in the form
	 *   [ '&' or '|' or '^' or '!', cond1, cond2, ... ]
	 * where cond1, cond2, ... are themselves conditions; *OR*
	 *   APCOND_EMAILCONFIRMED, *OR*
	 *   [ APCOND_EMAILCONFIRMED ], *OR*
	 *   [ APCOND_EDITCOUNT, number of edits ], *OR*
	 *   [ APCOND_AGE, seconds since registration ], *OR*
	 *   similar constructs defined by extensions.
	 * This function evaluates the former type recursively, and passes off to
	 * checkCondition for evaluation of the latter type.
	 *
	 * If you change the logic of this method, please update
	 * ApiQuerySiteinfo::appendAutoPromote(), as it depends on this method.
	 *
	 * @param mixed $cond A condition, possibly containing other conditions
	 * @param User $user The user to check the conditions against
	 *
	 * @return bool Whether the condition is true
	 */
	private function recCheckCondition( $cond, User $user ): bool {
		if ( is_array( $cond ) && count( $cond ) >= 2 && in_array( $cond[0], self::VALID_OPS ) ) {
			// Recursive condition
			if ( $cond[0] == '&' ) { // AND (all conds pass)
				foreach ( array_slice( $cond, 1 ) as $subcond ) {
					if ( !$this->recCheckCondition( $subcond, $user ) ) {
						return false;
					}
				}

				return true;
			} elseif ( $cond[0] == '|' ) { // OR (at least one cond passes)
				foreach ( array_slice( $cond, 1 ) as $subcond ) {
					if ( $this->recCheckCondition( $subcond, $user ) ) {
						return true;
					}
				}

				return false;
			} elseif ( $cond[0] == '^' ) { // XOR (exactly one cond passes)
				if ( count( $cond ) > 3 ) {
					$this->logger->warning(
						'recCheckCondition() given XOR ("^") condition on three or more conditions.' .
						' Check your $wgAutopromote and $wgAutopromoteOnce settings.'
					);
				}
				return $this->recCheckCondition( $cond[1], $user )
					xor $this->recCheckCondition( $cond[2], $user );
			} elseif ( $cond[0] == '!' ) { // NOT (no conds pass)
				foreach ( array_slice( $cond, 1 ) as $subcond ) {
					if ( $this->recCheckCondition( $subcond, $user ) ) {
						return false;
					}
				}

				return true;
			}
		}
		// If we got here, the array presumably does not contain other conditions;
		// it's not recursive. Pass it off to checkCondition.
		if ( !is_array( $cond ) ) {
			$cond = [ $cond ];
		}

		return $this->checkCondition( $cond, $user );
	}

	/**
	 * As recCheckCondition, but *not* recursive.  The only valid conditions
	 * are those whose first element is one of APCOND_* defined in Defines.php.
	 * Other types will throw an exception if no extension evaluates them.
	 *
	 * @param array $cond A condition, which must not contain other conditions
	 * @param User $user The user to check the condition against
	 * @return bool Whether the condition is true for the user
	 * @throws InvalidArgumentException if autopromote condition was not recognized.
	 * @throws LogicException if APCOND_BLOCKED is checked again before returning a result.
	 */
	private function checkCondition( array $cond, User $user ): bool {
		if ( count( $cond ) < 1 ) {
			return false;
		}

		switch ( $cond[0] ) {
			case APCOND_EMAILCONFIRMED:
				if ( Sanitizer::validateEmail( $user->getEmail() ) ) {
					if ( $this->options->get( MainConfigNames::EmailAuthentication ) ) {
						return (bool)$user->getEmailAuthenticationTimestamp();
					} else {
						return true;
					}
				}
				return false;
			case APCOND_EDITCOUNT:
				$reqEditCount = $cond[1] ?? $this->options->get( MainConfigNames::AutoConfirmCount );

				// T157718: Avoid edit count lookup if specified edit count is 0 or invalid
				if ( $reqEditCount <= 0 ) {
					return true;
				}
				return (int)$this->userEditTracker->getUserEditCount( $user ) >= $reqEditCount;
			case APCOND_AGE:
				$reqAge = $cond[1] ?? $this->options->get( MainConfigNames::AutoConfirmAge );
				$age = time() - (int)wfTimestampOrNull( TS_UNIX, $user->getRegistration() );
				return $age >= $reqAge;
			case APCOND_AGE_FROM_EDIT:
				$age = time() - (int)wfTimestampOrNull(
					TS_UNIX, $this->userEditTracker->getFirstEditTimestamp( $user ) );
				return $age >= $cond[1];
			case APCOND_INGROUPS:
				$groups = array_slice( $cond, 1 );
				return count( array_intersect( $groups, $this->getUserGroups( $user ) ) ) == count( $groups );
			case APCOND_ISIP:
				return $cond[1] == $user->getRequest()->getIP();
			case APCOND_IPINRANGE:
				return IPUtils::isInRange( $user->getRequest()->getIP(), $cond[1] );
			case APCOND_BLOCKED:
				// Because checking for ipblock-exempt leads back to here (thus infinite recursion),
				// we if we've been here before for this user without having returned a value.
				// See T270145 and T349608
				$userKey = $this->getCacheKey( $user );
				if ( $this->recursionMap[$userKey] ?? false ) {
					throw new LogicException(
						"Unexpected recursion! APCOND_BLOCKED is being checked during" .
						" an existing APCOND_BLOCKED check for \"{$user->getName()}\"!"
					);
				}
				$this->recursionMap[$userKey] = true;
				// Setting the second parameter here to true prevents us from getting back here
				// during standard MediaWiki core behavior
				$block = $user->getBlock( IDBAccessObject::READ_LATEST, true );
				$this->recursionMap[$userKey] = false;

				return $block && $block->isSitewide();
			case APCOND_ISBOT:
				return in_array( 'bot', $this->groupPermissionsLookup
					->getGroupPermissions( $this->getUserGroups( $user ) ) );
			default:
				$result = null;
				$this->hookRunner->onAutopromoteCondition( $cond[0],
					array_slice( $cond, 1 ), $user, $result );
				if ( $result === null ) {
					throw new InvalidArgumentException(
						"Unrecognized condition {$cond[0]} for autopromotion!"
					);
				}

				return (bool)$result;
		}
	}

	/**
	 * Add the user to the group if they meet given criteria.
	 *
	 * Contrary to autopromotion by $wgAutopromote, the group will be
	 *   possible to remove manually via Special:UserRights. In such case it
	 *   will not be re-added automatically. The user will also not lose the
	 *   group if they no longer meet the criteria.
	 *
	 * @param UserIdentity $user User to add to the groups
	 * @param string $event Key in $wgAutopromoteOnce (each event has groups/criteria)
	 *
	 * @return string[] Array of groups the user has been promoted to.
	 *
	 * @see $wgAutopromoteOnce
	 */
	public function addUserToAutopromoteOnceGroups(
		UserIdentity $user,
		string $event
	): array {
		$user->assertWiki( $this->wikiId );
		Assert::precondition(
			!$this->wikiId || WikiMap::isCurrentWikiDbDomain( $this->wikiId ),
			__METHOD__ . " is not supported for foreign wikis: {$this->wikiId} used"
		);

		if (
			$this->readOnlyMode->isReadOnly( $this->wikiId ) ||
			!$user->isRegistered() ||
			$this->tempUserConfig->isTempName( $user->getName() )
		) {
			return [];
		}

		$toPromote = $this->getUserAutopromoteOnceGroups( $user, $event );
		if ( $toPromote === [] ) {
			return [];
		}

		$userObj = User::newFromIdentity( $user );
		if ( !$userObj->checkAndSetTouched() ) {
			return []; // raced out (bug T48834)
		}

		$oldGroups = $this->getUserGroups( $user ); // previous groups
		$oldUGMs = $this->getUserGroupMemberships( $user );
		$this->addUserToMultipleGroups( $user, $toPromote );
		$newGroups = array_merge( $oldGroups, $toPromote ); // all groups
		$newUGMs = $this->getUserGroupMemberships( $user );

		// update groups in external authentication database
		// TODO: deprecate passing full User object to hook
		$this->hookRunner->onUserGroupsChanged(
			$userObj,
			$toPromote,
			[],
			false,
			false,
			$oldUGMs,
			$newUGMs
		);

		$logEntry = new ManualLogEntry( 'rights', 'autopromote' );
		$logEntry->setPerformer( $user );
		$logEntry->setTarget( $userObj->getUserPage() );
		$logEntry->setParameters( [
			'4::oldgroups' => $oldGroups,
			'5::newgroups' => $newGroups,
		] );
		$logid = $logEntry->insert();

		// Allow excluding autopromotions into select groups from RecentChanges (T377829).
		$groupsToShowInRC = array_diff(
			$toPromote,
			$this->options->get( MainConfigNames::AutopromoteOnceRCExcludedGroups )
		);

		if ( $this->options->get( MainConfigNames::AutopromoteOnceLogInRC ) && count( $groupsToShowInRC ) ) {
			$logEntry->publish( $logid );
		}

		return $toPromote;
	}

	/**
	 * Get the list of explicit group memberships this user has.
	 * The implicit * and user groups are not included.
	 *
	 * @param UserIdentity $user
	 * @param int $queryFlags
	 * @return string[]
	 */
	public function getUserGroups(
		UserIdentity $user,
		int $queryFlags = IDBAccessObject::READ_NORMAL
	): array {
		return array_keys( $this->getUserGroupMemberships( $user, $queryFlags ) );
	}

	/**
	 * Loads and returns UserGroupMembership objects for all the groups a user currently
	 * belongs to.
	 *
	 * @param UserIdentity $user the user to search for
	 * @param int $queryFlags
	 * @return UserGroupMembership[] Associative array of (group name => UserGroupMembership object)
	 */
	public function getUserGroupMemberships(
		UserIdentity $user,
		int $queryFlags = IDBAccessObject::READ_NORMAL
	): array {
		$user->assertWiki( $this->wikiId );
		$userKey = $this->getCacheKey( $user );

		if ( $this->canUseCachedValues( $user, self::CACHE_MEMBERSHIP, $queryFlags ) &&
			isset( $this->userGroupCache[$userKey][self::CACHE_MEMBERSHIP] )
		) {
			/** @suppress PhanTypeMismatchReturn */
			return $this->userGroupCache[$userKey][self::CACHE_MEMBERSHIP];
		}

		if ( !$user->isRegistered() ) {
			// Anon users don't have groups stored in the database
			return [];
		}

		$ugms = $this->store->getGroupMemberships( $user, $queryFlags );
		ksort( $ugms );

		$this->setCache( $userKey, self::CACHE_MEMBERSHIP, $ugms, $queryFlags );

		return $ugms;
	}

	/**
	 * Loads and returns UserGroupMembership objects for all the groups users currently
	 * belong to.
	 *
	 * @param array $userIds the user ids to search for
	 * @param int $queryFlags
	 * @return UserGroupMembership[][] Associative array of (user id => (group name => UserGroupMembership object))
	 */
	public function getUserGroupMembershipsFromUserIds(
		array $userIds,
		int $queryFlags = IDBAccessObject::READ_NORMAL
	): array {
		return $this->store->getGroupMembershipsFromUserIds( $userIds, $queryFlags );
	}

	/**
	 * Add the user to the given group. This takes immediate effect.
	 * If the user is already in the group, the expiry time will be updated to the new
	 * expiry time. (If $expiry is omitted or null, the membership will be altered to
	 * never expire.)
	 *
	 * @param UserIdentity $user
	 * @param string $group Name of the group to add
	 * @param string|null $expiry Optional expiry timestamp in any format acceptable to
	 *   wfTimestamp(), or null if the group assignment should not expire
	 * @param bool $allowUpdate Whether to perform "upsert" instead of INSERT
	 *
	 * @throws InvalidArgumentException
	 * @return bool
	 */
	public function addUserToGroup(
		UserIdentity $user,
		string $group,
		?string $expiry = null,
		bool $allowUpdate = false
	): bool {
		$user->assertWiki( $this->wikiId );
		if ( $this->readOnlyMode->isReadOnly( $this->wikiId ) ) {
			return false;
		}

		$isTemp = $this->tempUserConfig->isTempName( $user->getName() );
		if ( !$user->isRegistered() ) {
			throw new InvalidArgumentException(
				'UserGroupManager::addUserToGroup() needs a positive user ID. ' .
				'Perhaps addUserToGroup() was called before the user was added to the database.'
			);
		}
		if ( $isTemp ) {
			throw new InvalidArgumentException(
				'UserGroupManager::addUserToGroup() cannot be called on a temporary user.'
			);
		}

		if ( $expiry ) {
			$expiry = wfTimestamp( TS_MW, $expiry );
		}

		// TODO: Deprecate passing out user object in the hook by introducing
		// an alternative hook
		if ( $this->hookContainer->isRegistered( 'UserAddGroup' ) ) {
			$userObj = User::newFromIdentity( $user );
			$userObj->load();
			if ( !$this->hookRunner->onUserAddGroup( $userObj, $group, $expiry ) ) {
				return false;
			}
		}

		$oldUgms = $this->getUserGroupMemberships( $user, IDBAccessObject::READ_LATEST );
		$changed = $this->store->addGroup( $user, $group, $expiry, $allowUpdate );

		if ( $changed ) {
			$oldUgms[$group] = new UserGroupMembership( $user->getId( $this->wikiId ), $group, $expiry );
			if ( !$oldUgms[$group]->isExpired() ) {
				$this->setCache(
					$this->getCacheKey( $user ),
					self::CACHE_MEMBERSHIP,
					$oldUgms,
					IDBAccessObject::READ_LATEST
				);
				$this->clearUserCacheForKind( $user, self::CACHE_EFFECTIVE );
			}
			foreach ( $this->clearCacheCallbacks as $callback ) {
				$callback( $user );
			}
			return true;
		}
		return false;
	}

	/**
	 * Add the user to the given list of groups.
	 *
	 * @since 1.37
	 *
	 * @param UserIdentity $user
	 * @param string[] $groups Names of the groups to add
	 * @param string|null $expiry Optional expiry timestamp in any format acceptable to
	 *   wfTimestamp(), or null if the group assignment should not expire
	 * @param bool $allowUpdate Whether to perform "upsert" instead of INSERT
	 *
	 * @throws InvalidArgumentException
	 */
	public function addUserToMultipleGroups(
		UserIdentity $user,
		array $groups,
		?string $expiry = null,
		bool $allowUpdate = false
	) {
		foreach ( $groups as $group ) {
			$this->addUserToGroup( $user, $group, $expiry, $allowUpdate );
		}
	}

	/**
	 * Remove the user from the given group. This takes immediate effect.
	 *
	 * @param UserIdentity $user
	 * @param string $group Name of the group to remove
	 * @throws InvalidArgumentException
	 * @return bool
	 */
	public function removeUserFromGroup( UserIdentity $user, string $group ): bool {
		$user->assertWiki( $this->wikiId );
		// TODO: Deprecate passing out user object in the hook by introducing
		// an alternative hook
		if ( $this->hookContainer->isRegistered( 'UserRemoveGroup' ) ) {
			$userObj = User::newFromIdentity( $user );
			$userObj->load();
			if ( !$this->hookRunner->onUserRemoveGroup( $userObj, $group ) ) {
				return false;
			}
		}

		if ( $this->readOnlyMode->isReadOnly( $this->wikiId ) ) {
			return false;
		}

		if ( !$user->isRegistered() ) {
			throw new InvalidArgumentException(
				'UserGroupManager::removeUserFromGroup() needs a positive user ID. ' .
				'Perhaps removeUserFromGroup() was called before the user was added to the database.'
			);
		}

		$oldUgms = $this->getUserGroupMemberships( $user, IDBAccessObject::READ_LATEST );
		$oldFormerGroups = $this->getUserFormerGroups( $user, IDBAccessObject::READ_LATEST );
		if ( !$this->store->removeGroup( $user, $group ) ) {
			return false;
		}

		unset( $oldUgms[$group] );
		$userKey = $this->getCacheKey( $user );
		$this->setCache( $userKey, self::CACHE_MEMBERSHIP, $oldUgms, IDBAccessObject::READ_LATEST );
		$oldFormerGroups[] = $group;
		$this->setCache( $userKey, self::CACHE_FORMER, $oldFormerGroups, IDBAccessObject::READ_LATEST );
		$this->clearUserCacheForKind( $user, self::CACHE_EFFECTIVE );
		foreach ( $this->clearCacheCallbacks as $callback ) {
			$callback( $user );
		}
		return true;
	}

	/**
	 * Purge expired memberships from the user_groups table
	 * @internal
	 * @note this could be slow and is intended for use in a background job
	 * @return int|false false if purging wasn't attempted (e.g. because of
	 *  readonly), the number of rows purged (might be 0) otherwise
	 */
	public function purgeExpired() {
		if ( $this->readOnlyMode->isReadOnly( $this->wikiId ) ) {
			return false;
		}

		$purgedRows = $this->store->purgeExpired();

		return $purgedRows;
	}

	/**
	 * @param array $config
	 * @param string $group
	 * @return string[]
	 */
	private function expandChangeableGroupConfig( array $config, string $group ): array {
		if ( empty( $config[$group] ) ) {
			return [];
		} elseif ( $config[$group] === true ) {
			// You get everything
			return $this->listAllGroups();
		} elseif ( is_array( $config[$group] ) ) {
			return $config[$group];
		}
		return [];
	}

	/**
	 * Returns an array of the groups that a particular group can add/remove.
	 *
	 * @since 1.37
	 * @param string $group The group to check for whether it can add/remove
	 * @return array [
	 *     'add' => [ addablegroups ],
	 *     'remove' => [ removablegroups ],
	 *     'add-self' => [ addablegroups to self ],
	 *     'remove-self' => [ removable groups from self ] ]
	 */
	public function getGroupsChangeableByGroup( string $group ): array {
		return [
			'add' => $this->expandChangeableGroupConfig(
				$this->options->get( MainConfigNames::AddGroups ), $group
			),
			'remove' => $this->expandChangeableGroupConfig(
				$this->options->get( MainConfigNames::RemoveGroups ), $group
			),
			'add-self' => $this->expandChangeableGroupConfig(
				$this->options->get( MainConfigNames::GroupsAddToSelf ), $group
			),
			'remove-self' => $this->expandChangeableGroupConfig(
				$this->options->get( MainConfigNames::GroupsRemoveFromSelf ), $group
			),
		];
	}

	/**
	 * Returns an array of groups that this $actor can add and remove.
	 *
	 * @since 1.37
	 * @param Authority $authority
	 * @return array [
	 *  'add' => [ addablegroups ],
	 *  'remove' => [ removablegroups ],
	 *  'add-self' => [ addablegroups to self ],
	 *  'remove-self' => [ removable groups from self ]
	 * ]
	 * @phan-return array{add:list<string>,remove:list<string>,add-self:list<string>,remove-self:list<string>}
	 */
	public function getGroupsChangeableBy( Authority $authority ): array {
		if ( $authority->isAllowed( 'userrights' ) ) {
			// This group gives the right to modify everything (reverse-
			// compatibility with old "userrights lets you change
			// everything")
			// Using array_merge to make the groups reindexed
			$all = array_merge( $this->listAllGroups() );
			return [
				'add' => $all,
				'remove' => $all,
				'add-self' => [],
				'remove-self' => []
			];
		}

		// Okay, it's not so simple, we will have to go through the arrays
		$groups = [
			'add' => [],
			'remove' => [],
			'add-self' => [],
			'remove-self' => []
		];
		$actorGroups = $this->getUserEffectiveGroups( $authority->getUser() );

		foreach ( $actorGroups as $actorGroup ) {
			$groups = array_merge_recursive(
				$groups, $this->getGroupsChangeableByGroup( $actorGroup )
			);
			$groups['add'] = array_unique( $groups['add'] );
			$groups['remove'] = array_unique( $groups['remove'] );
			$groups['add-self'] = array_unique( $groups['add-self'] );
			$groups['remove-self'] = array_unique( $groups['remove-self'] );
		}
		return $groups;
	}

	/**
	 * Cleans cached group memberships for a given user
	 */
	public function clearCache( UserIdentity $user ) {
		$user->assertWiki( $this->wikiId );
		$userKey = $this->getCacheKey( $user );
		unset( $this->userGroupCache[$userKey] );
		unset( $this->queryFlagsUsedForCaching[$userKey] );
	}

	/**
	 * Sets cached group memberships and query flags for a given user
	 *
	 * @param string $userKey
	 * @param string $cacheKind one of self::CACHE_KIND_* constants
	 * @param array $groupValue
	 * @param int $queryFlags
	 */
	private function setCache(
		string $userKey,
		string $cacheKind,
		array $groupValue,
		int $queryFlags
	) {
		$this->userGroupCache[$userKey][$cacheKind] = $groupValue;
		$this->queryFlagsUsedForCaching[$userKey][$cacheKind] = $queryFlags;
	}

	/**
	 * Clears a cached group membership and query key for a given user
	 *
	 * @param UserIdentity $user
	 * @param string $cacheKind one of self::CACHE_* constants
	 */
	private function clearUserCacheForKind( UserIdentity $user, string $cacheKind ) {
		$userKey = $this->getCacheKey( $user );
		unset( $this->userGroupCache[$userKey][$cacheKind] );
		unset( $this->queryFlagsUsedForCaching[$userKey][$cacheKind] );
	}

	/**
	 * Gets a unique key for various caches.
	 * @param UserIdentity $user
	 * @return string
	 */
	private function getCacheKey( UserIdentity $user ): string {
		return $user->isRegistered() ? "u:{$user->getId( $this->wikiId )}" : "anon:{$user->getName()}";
	}

	/**
	 * Determines if it's ok to use cached options values for a given user and query flags
	 * @param UserIdentity $user
	 * @param string $cacheKind one of self::CACHE_* constants
	 * @param int $queryFlags
	 * @return bool
	 */
	private function canUseCachedValues(
		UserIdentity $user,
		string $cacheKind,
		int $queryFlags
	): bool {
		if ( !$user->isRegistered() ) {
			// Anon users don't have groups stored in the database,
			// so $queryFlags are ignored.
			return true;
		}
		if ( $queryFlags >= IDBAccessObject::READ_LOCKING ) {
			return false;
		}
		$userKey = $this->getCacheKey( $user );
		$queryFlagsUsed = $this->queryFlagsUsedForCaching[$userKey][$cacheKind] ?? IDBAccessObject::READ_NONE;
		return $queryFlagsUsed >= $queryFlags;
	}
}
