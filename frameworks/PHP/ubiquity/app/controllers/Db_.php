<?php
namespace controllers;

use Ubiquity\orm\DAO;
use controllers\utils\DbTrait;
use controllers\utils\DbAsyncTrait;

/**
 * Bench controller.
 */
class Db_ extends \Ubiquity\controllers\Controller {
	use DbTrait,DbAsyncTrait;

	public function index() {
		echo \json_encode(self::$pDao->execute([
			'id' => \mt_rand(1, 10000)
		])->_rest);
	}

	public function query($queries = 1) {
		$worlds = [];
		$count = $this->getCount($queries);

		while ($count --) {
			$worlds[] = (self::$pDao->execute([
				'id' => \mt_rand(1, 10000)
			]))->_rest;
		}
		echo \json_encode($worlds);
	}

	public function update($queries = 1) {
		$worlds = [];
		$count = $this->getCount($queries);

		while ($count --) {
			$world = self::$pDao->execute([
				'id' => \mt_rand(1, 10000)
			]);
			$world->randomNumber = \mt_rand(1, 10000);
			DAO::toUpdate($world);
			$worlds[] = $world->_rest;
		}
		DAO::flushUpdates();
		echo \json_encode($worlds);
	}
}
