<?php
namespace controllers;

use Ubiquity\orm\DAO;

/**
 * Bench controller.
 */
class DbMy extends Db_ {

	public function update($queries = 1) {
		$worlds = [];
		$count = \min(\max((int) $queries, 1), 500);
		$ids = $this->getUniqueRandomNumbers($count);
		foreach ($ids as $id) {
			$world = self::$pDao->execute([
				'id' => $id
			]);
			$world->randomNumber = \mt_rand(1, 10000);
			DAO::toUpdate($world);
			$worlds[] = $world->_rest;
		}
		DAO::updateGroups($count);
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
