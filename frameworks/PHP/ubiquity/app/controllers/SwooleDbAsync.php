<?php
namespace controllers;

use Ubiquity\orm\DAO;

/**
 * Bench controller.
 */
class SwooleDbAsync extends \Ubiquity\controllers\Controller {

	public function initialize() {
		\Ubiquity\utils\http\UResponse::setContentType('application/json');
	}

	public function index() {
		$dbInstance = DAO::pool('async');

		$world = DAO::executePrepared('world', [
			'id' => \mt_rand(1, 10000)
		]);
		DAO::freePool($dbInstance);
		echo \json_encode($world->_rest);
	}

	public function query($queries = 1) {
		$queries = \min(\max($queries, 1), 500);
		$worlds = [];
		$dbInstance = DAO::pool('async');
		for ($i = 0; $i < $queries; ++ $i) {
			$worlds[] = (DAO::executePrepared('world', [
				'id' => \mt_rand(1, 10000)
			]))->_rest;
		}
		DAO::freePool($dbInstance);

		echo \json_encode($worlds);
	}
}
