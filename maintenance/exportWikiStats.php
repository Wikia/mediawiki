<?php

declare( strict_types=1 );

use Fandom\CloseDeadWiki\WikiStats;
use Fandom\WikiConfig\WikiVariablesDataService;
use Fandom\WikiDomain\WikiConfigDataService;
use MediaWiki\MediaWikiServices;

require_once __DIR__ . '/Maintenance.php';

/**
 * Script export WikiStats:
 *  - Number of edits from six and four months
 *  - Number of active users from six and four months
 *  - Date of last edit in MAIN namespace
 *  - Number of content pages
 *  - Page views from 60 days
 *  - DB Size
 *  - Index Size
 *  - Config Values
 *
 * Each stat can be requested individually by flag
 *  - editStats - with this option exported data will include edits count and last edit data
 *  - activeUsers - with this option exported data will include active users numbers
 *  - dbSize - with this option exported data will db and index size
 *  - pageView - with this option exported data will include page views on the wiki
 *  - contentPages - with this option exported data will include number of the content pages
 *  - configOptions - this option allows to export wiki variables values. Each value should be separated by `,`
 *
 * Example command usage:
 *
 * php maintenance/exportWikiStats.php --input=stats.csv --output=result2.csv --editStats \
 * --configOptions="wgIsTestWiki,wgSitename,wgEnableEmail"
 *
 * Script as input file expects CSV formated file with two columns
 * where first value is Wiki ID and second is Wiki DB Name.
 * Example file data:
 * 147,starwars
 * 2233,enmarveldatabase
 * 831,muppet
 *
 * @ingroup Maintenance
 */
class ExportWikiStats extends Maintenance {
	private const EDITSTATS = 'editStats';
	private const ACTIVEUSERS = 'activeUsers';
	private const DBSIZE = 'dbSize';
	private const PAGEVIEW = 'pageView';
	private const CONTENTPAGES = 'contentPages';
	private const CONFIGOPTIONS = 'configOptions';
	private const PERIODS_OPTION = 'periods';
	private const LANGUAGE_OPTION = 'wikiLanguage';
	private const TOTALPAGESIZE = 'totalPageSize';
	private array $defaultPeriods = [ 2 ];

	private $requestedVariables = [];

	public function __construct() {
		parent::__construct();
		$this->addDescription( 'Evaluate wikis and returns their stats.' );
		$this->addOption( 'once', 'run the script for the first entry only (default: false)', false, false );
		$this->addOption( 'input', 'csv file with wiki ids and dbnames to check', true, true );
		$this->addOption( 'output', 'csv file to save results to', true, true );
		$this->addOption( self::EDITSTATS, 'should include last edit date and number of edits', false, false );
		$this->addOption( self::ACTIVEUSERS, 'should include active users stats', false, false );
		$this->addOption( self::DBSIZE, 'should include db and index size', false, false );
		$this->addOption( self::PAGEVIEW, 'should include page view stats', false, false );
		$this->addOption( self::CONTENTPAGES, 'should include content pages number', false, false );
		$this->addOption( self::CONFIGOPTIONS, 'should include specific config options', false, true );
		$this->addOption(
			self::PERIODS_OPTION,
			'allows to define periods (in months) for which data has to be gathered',
			false,
			true
		);
		$this->addOption( self::LANGUAGE_OPTION, 'should add language of the wiki', false, false );
		$this->addOption( self::TOTALPAGESIZE, 'should include total page size in bytes', false, false );
	}

	public function execute() {
		$wikiCnt = 0;

		$inputFile = $this->getOption( 'input' );
		$outputFile = $this->getOption( 'output' );
		$once = $this->getOption( 'once', false );

		$inHandle = fopen( $inputFile, "r" );
		if ( !$inHandle ) {
			$this->error( "could not open input CSV file $inputFile\n" );
			return false;
		}

		$outHandle = fopen( $outputFile, "w" );
		if ( !$outHandle ) {
			$this->error( "could not open output file $outputFile for writing\n" );
			fclose( $inHandle );
			return false;
		}

		$services = MediaWikiServices::getInstance();
		$variablesService = $services->getService( WikiVariablesDataService::class );
		$wikiConfigDataService = $services->getService( WikiConfigDataService::class );
		fputcsv(
			$outHandle,
			$this->prepareCSVHeader()
		);
		while ( ( $item = fgetcsv( $inHandle ) ) !== false ) {
			$wikiCnt++;
			if ( !is_numeric( $item[0] ) ) {
				$this->output( "skipping row [$wikiCnt] - not an valid wiki ID: $item[0]\n" );
				continue;
			}
			$wikiId = (int)$item[0];
			$wikiData = $wikiConfigDataService->getWikiDataById( $wikiId );
			if ( !$wikiData ) {
				$this->output( "Skipping [$wikiId] wiki not found\n" );
				continue;
			}
			$stats = new WikiStats( $wikiData );

			$out = [];
			if ( $this->getOption( self::EDITSTATS ) ) {
				foreach ( $this->getPeriods() as $period ) {
					$out[] = $stats->getWikiEditCount( sprintf( '-%d months', $period ) );
				}
				$out[] = $stats->getWikiLastEdit();
			}

			if ( $this->getOption( self::ACTIVEUSERS ) ) {
				foreach ( $this->getPeriods() as $period ) {
					$out[] = $stats->getWikiActiveUsers( sprintf( '-%d months', $period ) );
				}
			}

			if ( $this->getOption( self::PAGEVIEW ) ) {
				foreach ( $this->getPeriods() as $period ) {
					$out[] = $stats->getPageViews( sprintf( '-%d months', $period ) );
				}
			}

			if ( $this->getOption( self::DBSIZE ) ) {
				$dbSizes = $stats->getDBAndIndexSize();
				$out[] = $dbSizes['data_length'];
				$out[] = $dbSizes['index_length'];
			}

			if ( $this->getOption( self::CONTENTPAGES ) ) {
				$out[] = $stats->getContentPagesCount();
			}

			if ( $this->getOption( self::TOTALPAGESIZE ) ) {
				$out[] = $stats->getTotalPageSize();
			}

			if ( $this->hasOption( self::CONFIGOPTIONS ) ) {
				// Check requested variables in Wiki configuration and return values for them
				$wikiVariables = $variablesService->getVariablesForWiki( $wikiId, false );
				foreach ( $this->requestedVariables as $variable ) {
					$out[] = $wikiVariables[$variable] ?? '';
				}
			}

			if ( $this->hasOption( self::LANGUAGE_OPTION ) ) {
				$out[] = $wikiData->getLangCode();
			}

			fputcsv( $outHandle, array_merge( $item, $out ) );

			$this->output( "[$wikiCnt] [$wikiId] finished exporting stats\n" );

			// Pause the script after 10 wikis, to not ddos mysql servers with queries (especialy "I" cluster)
			if ( $wikiCnt % 10 === 0 ) {
				sleep( 1 );
			}
			if ( $once ) {
				break;
			}
		}

		fclose( $inHandle );
		fclose( $outHandle );

		$this->output( "Export wiki stats script finished, checked $wikiCnt wikis\n" );

		return true;
	}

	/**
	 * Returns list of headers for requested stats
	 *
	 * @return string[]
	 */
	private function prepareCSVHeader(): array {
		$header = [
			'ID',
			'DB Name',
		];

		if ( $this->getOption( self::EDITSTATS ) ) {
			foreach ( $this->getPeriods() as $period ) {
				$header[] = sprintf( 'edits (-%d months)', $period );
			}
			$header[] = 'last edit date';
		}

		if ( $this->getOption( self::ACTIVEUSERS ) ) {
			foreach ( $this->getPeriods() as $period ) {
				$header[] = sprintf( 'active users (-%d months)', $period );
			}
		}

		if ( $this->getOption( self::PAGEVIEW ) ) {
			foreach ( $this->getPeriods() as $period ) {
				$header[] = sprintf( 'page views (-%d months)', $period );
			}
		}

		if ( $this->getOption( self::DBSIZE ) ) {
			$header[] = 'database size';
			$header[] = 'database index size';
		}

		if ( $this->getOption( self::CONTENTPAGES ) ) {
			$header[] = 'content pages';
		}

		if ( $this->getOption( self::TOTALPAGESIZE ) ) {
			$header[] = 'total page size (B)';
		}

		if ( $this->hasOption( self::CONFIGOPTIONS ) ) {
			$configOption = $this->getOption( self::CONFIGOPTIONS, '' );
			$configVariables = explode( ',', $configOption );
			foreach ( $configVariables as $variable ) {
				$this->requestedVariables[] = $variable;
				$header[] = $variable;
			}
		}

		if ( $this->hasOption( self::LANGUAGE_OPTION ) ) {
			$header[] = 'wiki\'s language';
		}

		return $header;
	}

	private function getPeriods(): array {
		if ( $this->hasOption( self::PERIODS_OPTION ) ) {
			return explode( ",", $this->getOption( self::PERIODS_OPTION ) );
		}

		return $this->defaultPeriods;
	}
}

$maintClass = ExportWikiStats::class;
require_once RUN_MAINTENANCE_IF_MAIN;
