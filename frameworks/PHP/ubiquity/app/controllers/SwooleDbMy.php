<?php
namespace controllers;

use Ubiquity\orm\DAO;

/**
 * Bench controller.
 */
class SwooleDbMy extends SwooleDb {

	public function update($queries = 1) {
		$worlds = [];
		$queries = \min(\max($queries, 1), 500);
		$ids = $this->getUniqueRandomNumbers($queries);
		foreach ($ids as $id) {
			$world = DAO::executePrepared('world', [
				'id' => $id
			]);
			$world->randomNumber = \mt_rand(1, 10000);
			DAO::toUpdate($world);
			$worlds[] = $world->_rest;
		}
		DAO::updateGroups($queries);
		echo \json_encode($worlds);
	}

	private function getUniqueRandomNumbers($count) {
		$res = [];
		do {
			$res[\mt_rand(1, 10000)] = 1;
		} while (\count($res) < $count);

		\ksort($res);

		return \array_keys($res);
	}
}
