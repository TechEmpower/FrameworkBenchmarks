<?php
namespace controllers;

use Ubiquity\controllers\Startup;
use Ubiquity\utils\http\UResponse;
use Ubiquity\orm\DAO;
use models\World;

/**
 * Bench controller.
 */
class Db_ extends Db {

	public function initialize() {
		UResponse::setContentType('application/json');
		$config = Startup::getConfig();
		DAO::startDatabase($config);
	}

	public function update($queries = 1) {
		$worlds = [];
		$queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;
		for ($i = 0; $i < $queries; ++ $i) {
			$world = DAO::getById(World::class, mt_rand(1, 10000), false);
			$world->setRandomNumber(mt_rand(1, 10000));
			DAO::update($world);
			$worlds[] = $world->_rest;
		}
		echo \json_encode($worlds);
	}
}
