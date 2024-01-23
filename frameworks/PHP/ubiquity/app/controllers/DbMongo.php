<?php
namespace controllers;

use Ubiquity\orm\DAONosql;
use Ubiquity\orm\core\prepared\DAONosqlPreparedQueryById;
use models\World;

/**
 * Bench controller.
 */
class DbMongo extends Db_ {

	public static function warmup() {
		self::$pDao = new DAONosqlPreparedQueryById('models\\World');
	}

	public function update($queries = 1) {
		$worlds = [];
		$count = \min(\max((int) $queries, 1), 500);
		$ids = $this->getUniqueRandomNumbers($count);
		foreach ($ids as $id) {
			$world = self::$pDao->execute([
				'id' => $id
			]);
			do {
				$nRn = \mt_rand(1, 10000);
			} while ($world->randomNumber == $nRn);
			$world->randomNumber = $nRn;
			DAONosql::toUpdate($world);
			$worlds[] = $world->_rest;
		}
		DAONosql::flush(World::class, false);

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
