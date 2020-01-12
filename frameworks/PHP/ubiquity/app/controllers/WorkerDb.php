<?php
namespace controllers;

use Ubiquity\orm\DAO;
use models\World;

/**
 * Bench controller.
 */
class WorkerDb extends \Ubiquity\controllers\Controller {

	public function __construct() {}

	public function initialize() {
		\Ubiquity\utils\http\UResponse::setContentType('application/json');
	}

	public function index() {
		$world = DAO::getById(World::class, [
			'id' => \mt_rand(1, 10000)
		], false);
		echo \json_encode($world->_rest);
	}

	public function query($queries = 1) {
		$worlds = [];
		$queries = \min(\max($queries, 1), 500);
		for ($i = 0; $i < $queries; ++ $i) {
			$worlds[] = (DAO::getById(World::class, [
				'id' => \mt_rand(1, 10000)
			], false))->_rest;
		}
		echo \json_encode($worlds);
	}

	public function update($queries = 1) {
		$worlds = [];
		$queries = \min(\max($queries, 1), 500);
		for ($i = 0; $i < $queries; ++ $i) {
			$world = DAO::getById(World::class, [
				'id' => \mt_rand(1, 10000)
			], false);
			$world->randomNumber = \mt_rand(1, 10000);
			DAO::toUpdate($world);
			$worlds[] = $world->_rest;
		}
		DAO::flushUpdates();
		echo \json_encode($worlds);
	}
}
