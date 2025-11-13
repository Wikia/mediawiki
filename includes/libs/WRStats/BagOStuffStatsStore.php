<?php

namespace Wikimedia\WRStats;

use Fandom\Includes\Logging\Loggable;
use Wikimedia\ObjectCache\BagOStuff;

/**
 * An adaptor allowing WRStats to store data in MediaWiki's BagOStuff
 *
 * @newable
 * @since 1.39
 */
class BagOStuffStatsStore implements StatsStore {
	use Loggable;
	/** @var BagOStuff */
	private $cache;

	/**
	 * @param BagOStuff $cache
	 */
	public function __construct( BagOStuff $cache ) {
		$this->cache = $cache;
	}

	/**
	 * @inheritDoc
	 * @suppress PhanParamTooFewUnpack
	 */
	public function makeKey( $prefix, $internals, $entity ) {
		if ( $entity->isGlobal() ) {
			return $this->cache->makeGlobalKey(
				...$prefix, ...$internals, ...$entity->getComponents() );
		} else {
			return $this->cache->makeKey(
				...$prefix, ...$internals, ...$entity->getComponents() );
		}
	}

	public function incr( array $values, $ttl ) {
		foreach ( $values as $key => $value ) {
			$this->error( 'key:' . $key . ' incr by ' . $value . ' ttl:' . $ttl );
			$this->cache->incrWithInit(
				$key,
				$ttl,
				$value,
				$value,
				BagOStuff::WRITE_BACKGROUND
			);
		}
	}

	public function delete( array $keys ) {
		$this->error( 'Deleting keys: ' . implode( ', ', $keys ) );
		$this->cache->deleteMulti( $keys, BagOStuff::WRITE_BACKGROUND );
	}

	public function query( array $keys ) {
		$this->error( 'Querying keys: ' . implode( ', ', $keys ) );
		return $this->cache->getMulti( $keys );
	}
}
