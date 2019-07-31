<?php
namespace controllers;

use Ubiquity\controllers\Controller;
use Ubiquity\db\Database;

/**
 * Bench controller.
 */
class Raw extends Controller {
	protected $db;
	public function initialize() {
		\header('Content-type: application/json');
		$this->db=Database::start();
	}
	
	public function index() {
		echo \json_encode($this->getRandomWorld(\mt_rand(1, 10000)));
	}
	
	public function query($queries = 1) {
		$worlds = [];
		$queries = \min(\max($queries, 1), 500);
		for ($i = 0; $i < $queries; ++ $i) {
			$worlds[] = $this->getRandomWorld(\mt_rand(1, 10000));
		}
		echo \json_encode($worlds);
	}
	
	public function update($queries = 1) {
		$worlds = [];
		$queries = \min(\max($queries, 1), 500);
		for ($i = 0; $i < $queries; ++ $i) {
			$world = $this->getRandomWorld(\mt_rand(1, 10000));
			$this->db->prepareAndExecuteUpdate("UPDATE World SET randomNumber = ? WHERE id = ?",[$world['randomNumber'] = \mt_rand(1, 10000),$world['id']]);
			$worlds[] = $world;
		}
		echo \json_encode($worlds);
	}
	
	private function getRandomWorld($id) {
		return $this->db->prepareAndFetchOne('SELECT id,randomNumber FROM World WHERE id = ? LIMIT 1', [
			$id
		],\PDO::FETCH_ASSOC);
	}
}


