<?php
namespace controllers;

use Ubiquity\controllers\Controller;
use Ubiquity\orm\DAO;
use models\World;

/**
 * Bench controller.
 **/
class Bench extends Controller{
	public function index() {
	}
	
	public function plaintext(){
		\header("Content-Type: text/plain");
		echo "Hello, World!";
	}
	public function json(){
		\header( 'Content-Type', 'application/json' );
		echo \json_encode(['message' => 'Hello, World!']);
	}
	
	public function db() {
		\header( 'Content-Type', 'application/json' );
		$world=DAO::getOne(World::class, mt_rand(1, 10000),false);
		echo json_encode($world->_rest);
	}
	
	public function dbquery($queries = 1) {
		\header( 'Content-Type', 'application/json' );
		$worlds = array();
		$queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;
		for ($i = 0; $i < $queries; ++$i) {
			$world=DAO::getOne(World::class, mt_rand(1, 10000),false);
			$worlds[]=$world->_rest;
		}
		echo json_encode($worlds);
	}
}
