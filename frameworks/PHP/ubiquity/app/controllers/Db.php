<?php
namespace controllers;

use Ubiquity\controllers\Controller;
use Ubiquity\orm\DAO;
use models\World;
use Ubiquity\controllers\Startup;
use Ubiquity\utils\http\UResponse;

/**
 * Bench controller.
 */
class Db extends Controller {

	public function initialize() {
		UResponse::setContentType('application/json');
	}

	public function index() {
		$world = DAO::getById(World::class, mt_rand(1, 10000), false);
		echo \json_encode($world->_rest);
	}

	public function query($queries = 1) {
		$worlds = [];
		$queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;
		for ($i = 0; $i < $queries; ++ $i) {
			$world = DAO::getById(World::class, mt_rand(1, 10000), false);
			$worlds[] = $world->_rest;
		}
		echo \json_encode($worlds);
	}

	public function update($queries = 1) {
		$worlds = [];
		$queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;
		DAO::beginTransaction();
		for ($i = 0; $i < $queries; ++ $i) {
			$world = DAO::getById(World::class, mt_rand(1, 10000), false);
			$world->setRandomNumber(mt_rand(1, 10000));
			DAO::update($world);
			$worlds[] = $world->_rest;
		}
		DAO::commit();
		echo \json_encode($worlds);
	}
}
