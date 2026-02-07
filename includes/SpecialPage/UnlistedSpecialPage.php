<?php
/**
 * Shortcut to construct a special page which is unlisted by default.
 *
 * @license GPL-2.0-or-later
 * @file
 * @ingroup SpecialPage
 */

namespace MediaWiki\SpecialPage;

/**
 * Shortcut to construct a special page which is unlisted by default.
 *
 * @stable to extend
 *
 * @ingroup SpecialPage
 */
class UnlistedSpecialPage extends SpecialPage {

	/**
	 * @stable to call
	 *
	 * @param string $name
	 * @param string $restriction
	 * @param bool $function Unused. Deprecated since 1.46.
	 * @param string $file Unused. Deprecated since 1.46.
	 */
	public function __construct( $name, $restriction = '', $function = false, $file = 'default' ) {
		if ( static::class === self::class ) {
			wfDeprecated( 'direct instantiation of ' . __CLASS__, '1.46' );
		}
		if ( func_num_args() > 2 ) {
			wfDeprecated( __CLASS__ . ' constructor parameters $function and $file', '1.46' );
		}
		parent::__construct( $name, $restriction, false );
	}

	/** @inheritDoc */
	public function isListed() {
		return false;
	}
}

/** @deprecated class alias since 1.41 */
class_alias( UnlistedSpecialPage::class, 'UnlistedSpecialPage' );
