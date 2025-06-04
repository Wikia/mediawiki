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

use MediaWiki\Deferred\DeferredUpdates;
use MediaWiki\JobQueue\JobQueueGroup;
use stdClass;
use UserGroupExpiryJob;
use Wikimedia\Rdbms\IConnectionProvider;
use Wikimedia\Rdbms\IDBAccessObject;
use Wikimedia\Rdbms\IReadableDatabase;
use Wikimedia\Rdbms\SelectQueryBuilder;

/**
 * Manage user group storage
 *
 * @since 1.44
 * @ingroup User
 */
class UserGroupStore {

	public function __construct(
		private readonly IConnectionProvider $dbProvider,
		private readonly JobQueueGroup $jobQueueGroup,
		/** @var string|false */
		private readonly string|false $wikiId = UserIdentity::LOCAL
	) {
	}

	/**
	 * Creates a new UserGroupMembership instance from $row.
	 * The fields required to build an instance could be
	 * found using getQueryInfo() method.
	 *
	 * @param stdClass $row A database result object
	 *
	 * @return UserGroupMembership
	 */
	public function newGroupMembershipFromRow( stdClass $row ): UserGroupMembership {
		return new UserGroupMembership(
			(int)$row->ug_user,
			$row->ug_group,
			$row->ug_expiry === null ? null : wfTimestamp(
				TS_MW,
				$row->ug_expiry
			)
		);
	}

	/**
	 * Loads and returns UserGroupMembership objects for all the groups a user currently
	 * belongs to.
	 *
	 * @param UserIdentity $user the user to search for
	 * @param int $queryFlags
	 * @return UserGroupMembership[] Associative array of (group name => UserGroupMembership object)
	 */
	public function getGroupMemberships(
		UserIdentity $user,
		int $queryFlags = IDBAccessObject::READ_NORMAL
	): array {
		$res = $this->newQueryBuilder( $this->getConnectionForQueryFlags( $queryFlags ) )
			->where( [ 'ug_user' => $user->getId( $this->wikiId ) ] )
			->caller( __METHOD__ )
			->fetchResultSet();

		$ugms = [];
		foreach ( $res as $row ) {
			$ugm = $this->newGroupMembershipFromRow( $row );
			if ( !$ugm->isExpired() ) {
				$ugms[$ugm->getGroup()] = $ugm;
			}
		}

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
	public function getGroupMembershipsFromUserIds(
		array $userIds,
		int $queryFlags = IDBAccessObject::READ_NORMAL
	): array {
		// Lookup groups for all the users
		$res = $this->newQueryBuilder( $this->getConnectionForQueryFlags( $queryFlags ) )
			->where( [ 'ug_user' => $userIds ] )
			->caller( __METHOD__ )
			->fetchResultSet();

		$ugmsByUserId = [];
		foreach ( $res as $row ) {
			$ugm = $this->newGroupMembershipFromRow( $row );
			if ( !$ugm->isExpired() ) {
				$ugmsByUserId[$row->ug_user][$row->ug_group] = $ugm;
			}
		}

		return $ugmsByUserId;
	}

	/**
	 * Add the user to the given group. This takes immediate effect.
	 *  If the user is already in the group, the expiry time will be updated to the new
	 *  expiry time. (If $expiry is omitted or null, the membership will be altered to
	 *  never expire.)
	 *
	 * @param UserIdentity $user
	 * @param string $group Name of the group to add
	 * @param string|null $expiry Optional expiry timestamp in any format acceptable to
	 *    wfTimestamp(), or null if the group assignment should not expire
	 * @param bool $allowUpdate Whether to perform "upsert" instead of INSERT
	 * @return bool
	 */
	public function addGroup(
		UserIdentity $user, string $group, string|null $expiry, bool $allowUpdate
	): bool {
		$dbw = $this->dbProvider->getPrimaryDatabase( $this->wikiId );

		$dbw->startAtomic( __METHOD__ );
		$dbw->newInsertQueryBuilder()->insertInto( 'user_groups' )->ignore()->row( [
			'ug_user' => $user->getId( $this->wikiId ),
			'ug_group' => $group,
			'ug_expiry' => $expiry ? $dbw->timestamp( $expiry ) : null,
		] )->caller( __METHOD__ )->execute();

		$affected = $dbw->affectedRows();
		if ( !$affected ) {
			// Conflicting row already exists; it should be overridden if it is either expired
			// or if $allowUpdate is true and the current row is different than the loaded row.
			$conds = [
				'ug_user' => $user->getId( $this->wikiId ),
				'ug_group' => $group,
			];
			if ( $allowUpdate ) {
				// Update the current row if its expiry does not match that of the loaded row
				$conds[] = $expiry
					? $dbw->expr( 'ug_expiry', '=', null )
						->or( 'ug_expiry', '!=', $dbw->timestamp( $expiry ) )
					: $dbw->expr( 'ug_expiry', '!=', null );
			} else {
				// Update the current row if it is expired
				$conds[] = $dbw->expr( 'ug_expiry', '<', $dbw->timestamp() );
			}
			$dbw->newUpdateQueryBuilder()
				->update( 'user_groups' )
				->set( [
					'ug_expiry' => $expiry ? $dbw->timestamp( $expiry ) : null,
				] )
				->where( $conds )
				->caller( __METHOD__ )
				->execute();
			$affected = $dbw->affectedRows();
		}
		$dbw->endAtomic( __METHOD__ );

		// Purge old, expired memberships from the DB
		DeferredUpdates::addCallableUpdate( function ( $fname ) {
			$dbr = $this->dbProvider->getReplicaDatabase( $this->wikiId );
			$hasExpiredRow = (bool)$dbr->newSelectQueryBuilder()
				->select( '1' )
				->from( 'user_groups' )
				->where( [ $dbr->expr( 'ug_expiry', '<', $dbr->timestamp() ) ] )
				->caller( $fname )
				->fetchField();
			if ( $hasExpiredRow ) {
				$this->jobQueueGroup->push( new UserGroupExpiryJob( [] ) );
			}
		} );

		return $affected > 0;
	}

	/**
	 * Remove the user from the given group. This takes immediate effect.
	 *
	 * @param UserIdentity $user
	 * @param string $group Name of the group to remove
	 * @return bool
	 */
	public function removeGroup( UserIdentity $user, string $group ): bool {
		$dbw = $this->dbProvider->getPrimaryDatabase( $this->wikiId );
		$dbw->newDeleteQueryBuilder()
			->deleteFrom( 'user_groups' )
			->where( [
				'ug_user' => $user->getId( $this->wikiId ),
				'ug_group' => $group,
			] )
			->caller( __METHOD__ )
			->execute();

		if ( !$dbw->affectedRows() ) {
			return false;
		}

		// Remember that the user was in this group
		$dbw->newInsertQueryBuilder()
			->insertInto( 'user_former_groups' )
			->ignore()
			->row( [
				'ufg_user' => $user->getId( $this->wikiId ),
				'ufg_group' => $group,
			] )
			->caller( __METHOD__ )
			->execute();

		return true;
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
	public function getFormerGroups(
		UserIdentity $user,
		int $queryFlags = IDBAccessObject::READ_NORMAL
	): array {
		return $this->getConnectionForQueryFlags( $queryFlags )
			->newSelectQueryBuilder()
			->select( 'ufg_group' )
			->from( 'user_former_groups' )
			->where( [ 'ufg_user' => $user->getId( $this->wikiId ) ] )
			->caller( __METHOD__ )
			->fetchFieldValues();
	}

	/**
	 * Purge expired memberships from the user_groups table
	 * @internal
	 * @note this could be slow and is intended for use in a background job
	 * @return int|false false if lock cannot be acquired,
	 *   the number of rows purged (might be 0) otherwise
	 */
	public function purgeExpired(): int|false {
		$ticket = $this->dbProvider->getEmptyTransactionTicket( __METHOD__ );
		$dbw = $this->dbProvider->getPrimaryDatabase( $this->wikiId );

		$lockKey = "{$dbw->getDomainID()}:UserGroupManager:purge"; // per-wiki
		$scopedLock = $dbw->getScopedLockAndFlush( $lockKey, __METHOD__, 0 );
		if ( !$scopedLock ) {
			return false; // already running
		}

		$now = time();
		$purgedRows = 0;
		do {
			$dbw->startAtomic( __METHOD__ );
			$res = $this->newQueryBuilder( $dbw )
				->where( [
					$dbw->expr( 'ug_expiry', '<', $dbw->timestamp( $now ) ),
				] )
				->forUpdate()
				->limit( 100 )
				->caller( __METHOD__ )
				->fetchResultSet();

			if ( $res->numRows() > 0 ) {
				$insertData = []; // array of users/groups to insert to user_former_groups
				$deleteCond = []; // array for deleting the rows that are to be moved around
				foreach ( $res as $row ) {
					$insertData[] = [ 'ufg_user' => $row->ug_user, 'ufg_group' => $row->ug_group ];
					$deleteCond[] = $dbw->expr( 'ug_user', '=', $row->ug_user )
						->and( 'ug_group', '=', $row->ug_group );
				}
				// Delete the rows we're about to move
				$dbw->newDeleteQueryBuilder()
					->deleteFrom( 'user_groups' )
					->where( $dbw->orExpr( $deleteCond ) )
					->caller( __METHOD__ )
					->execute();
				// Push the groups to user_former_groups
				$dbw->newInsertQueryBuilder()
					->insertInto( 'user_former_groups' )
					->ignore()
					->rows( $insertData )
					->caller( __METHOD__ )
					->execute();
				// Count how many rows were purged
				$purgedRows += $res->numRows();
			}

			$dbw->endAtomic( __METHOD__ );

			$this->dbProvider->commitAndWaitForReplication( __METHOD__, $ticket );
		} while ( $res->numRows() > 0 );

		return $purgedRows;
	}

	/**
	 * Return the query builder to build upon and query
	 *
	 * @param IReadableDatabase $db
	 * @return SelectQueryBuilder
	 * @internal
	 */
	private function newQueryBuilder( IReadableDatabase $db ): SelectQueryBuilder {
		return $db->newSelectQueryBuilder()
			->select( [
				'ug_user',
				'ug_group',
				'ug_expiry',
			] )
			->from( 'user_groups' );
	}

	/**
	 * @param int $recency a bit field composed of IDBAccessObject::READ_XXX flags
	 * @return IReadableDatabase
	 */
	private function getConnectionForQueryFlags( int $recency ): IReadableDatabase {
		if ( ( IDBAccessObject::READ_LATEST & $recency ) == IDBAccessObject::READ_LATEST ) {
			return $this->dbProvider->getPrimaryDatabase( $this->wikiId );
		}

		return $this->dbProvider->getReplicaDatabase( $this->wikiId );
	}
}
