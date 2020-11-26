<?php
namespace controllers;

use Ubiquity\orm\SDAO;
use models\World;
use controllers\utils\DbTrait;

/**
 * Bench controller.
 */
class Db extends \Ubiquity\controllers\Controller {
	use DbTrait;

	public function __construct() {}

	public function initialize() {
		\header('Content-Type: application/json');
		\Ubiquity\cache\CacheManager::startProdFromCtrl();
	}

	public function index() {
		echo \json_encode((SDAO::getById(World::class, [
			'id' => \mt_rand(1, 10000)
		], false))->_rest);
	}

	public function query($queries = 1) {
		$worlds = [];
		$count = $this->getCount($queries);
		for ($i = 0; $i < $count; ++ $i) {
			$worlds[] = (SDAO::getById(World::class, [
				'id' => \mt_rand(1, 10000)
			], false))->_rest;
		}
		echo \json_encode($worlds);
	}

	public function update($queries = 1) {
		$worlds = [];

		$count = $this->getCount($queries);
		$ids = $this->getUniqueRandomNumbers($count);
		foreach ($ids as $id) {
			$world = SDAO::getById(World::class, [
				'id' => $id
			], false);
			$world->randomNumber = \mt_rand(1, 10000);
			SDAO::toUpdate($world);
			$worlds[] = $world->_rest;
		}
		SDAO::updateGroups($count);

		echo \json_encode($worlds);
	}

	private function getUniqueRandomNumbers($count) {
		$res = [];
		do {
			$res[\mt_rand(1, 10000)] = 1;
		} while (\count($res) < $count);

		\ksort($res); // prevent deadlocks (see https://github.com/TechEmpower/FrameworkBenchmarks/pull/5230#discussion_r345780701)

		return \array_keys($res);
	}
}
