<?php
namespace controllers;

use controllers\utils\DbTrait;

/**
 * Bench controller.
 */
class Cache extends \Ubiquity\controllers\Controller {
	use DbTrait;

	protected $cache;

	public function __construct() {
		$this->cache = \Ubiquity\orm\DAO::getCache();
	}

	public function initialize() {
		\Ubiquity\utils\http\UResponse::setContentType('application/json');
	}

	public function index() {}

	public function cachedquery($queries = 1) {
		$worlds = [];
		$count = $this->getCount($queries);
		while ($count --) {
			$worlds[] = ($this->cache->fetch('models\\CachedWorld', \mt_rand(1, 10000)))->_rest;
		}
		echo \json_encode($worlds);
	}
}
