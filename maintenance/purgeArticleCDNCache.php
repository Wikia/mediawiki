<?php

declare(strict_types=1);

use Fandom\Includes\Config\FandomConfigNames;
use GuzzleHttp\Exception\GuzzleException;
use GuzzleHttp\RequestOptions;

// @codeCoverageIgnoreStart
require_once __DIR__ . '/Maintenance.php';
// @codeCoverageIgnoreEnd

class PurgeArticleCDNCache extends Maintenance
{
	public function __construct()
	{
		parent::__construct();
		$this->addDescription('Purge Article CDN Cache using Pandora\'s cdn-purger');
		$this->addOption('urls', 'List of pipe separated url addresses', true, true);
	}

	/**
	 * @inheritDoc
	 */
	public function execute(): void
	{
		$urlsString = $this->getOption('urls', '');
		if ('' === $urlsString) {
			$this->output("Urls list cannot be empty.");
			return;
		}

		$urls = explode('|', $urlsString);

		$this->purgeUrls($urls);
	}

	/**
	 * @param string[] $urls
	 */
	private function purgeUrls(array $urls): void
	{
		$this->output(sprintf("Purging %s urls\n", count($urls)));

		$httpClient = $this->getServiceContainer()->getHttpRequestFactory()->createGuzzleClient();
		try {
			$response = $httpClient->post(
				$this->getEndpointUrl(),
				$this->buildRequestOptions($urls)
			);


			if (200 === $response->getStatusCode()) {
				$this->output("Done!\n");
				return;
			}
		} catch (GuzzleException) {
		}

		$this->error("Cache purge failed\n");
	}

	private function getEndpointUrl(): string
	{
		return $this->getServiceContainer()->getMainConfig()->get(FandomConfigNames::PurgeCDNCacheURL);
	}

	/**
	 * @param array $urls
	 * @return array
	 */
	public function buildRequestOptions(array $urls): array
	{
		return [
			RequestOptions::BODY => json_encode([
				'urls' => $urls,
			]),
			RequestOptions::HEADERS => [
				'Content-Type' => 'application/json',
				'X-Wikia-Internal-Request' => '1',
			],
		];
	}
}

// @codeCoverageIgnoreStart
$maintClass = PurgeArticleCDNCache::class;
require_once RUN_MAINTENANCE_IF_MAIN;
// @codeCoverageIgnoreEnd
