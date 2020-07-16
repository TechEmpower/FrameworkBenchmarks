<?php
namespace controllers;


/**
 * Bench controller.
 */
class Cached extends \Ubiquity\controllers\Controller {

	protected $cache;

	public function __construct() {
		global $cache;
		$this->cache=$cache;
	}

	public function initialize() {
		\Ubiquity\utils\http\UResponse::setContentType('application/json');
	}

	public function index(){}

	public function query($queries = 1) {
		$worlds = [];
		$queries = \min(\max($queries, 1), 500);
		for ($i = 0; $i < $queries; ++ $i) {
			$worlds[] = ($this->cache->fetch('models\\CachedWorld', [\mt_rand(1, 10000)]))->_rest;
		}
		echo \json_encode($worlds);
	}
}
