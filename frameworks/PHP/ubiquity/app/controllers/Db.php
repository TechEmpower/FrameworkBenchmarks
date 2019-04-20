<?php
namespace controllers;

use Ubiquity\controllers\Controller;
use Ubiquity\orm\DAO;
use models\World;
use Ubiquity\controllers\Startup;

/**
 * Bench controller.
 **/
class Db extends Controller{
	public function initialize(){
		\header( 'Content-Type', 'application/json' );
		$config=Startup::getConfig();
		\Ubiquity\orm\DAO::startDatabase($config);
	}

	public function index() {
		$world=DAO::getOne(World::class, mt_rand(1, 2),false);
		echo json_encode($world->_rest);
	}
	
	public function query($queries = 1) {
		$worlds = [];
		$queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;
		for ($i = 0; $i < $queries; ++$i) {
			$world=DAO::getOne(World::class, mt_rand(1, 10000),false);
			$worlds[]=$world->_rest;
		}
		echo json_encode($worlds);
	}
	
	public function update($queries = 1) {
		$worlds = [];
		$queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;
		for ($i = 0; $i < $queries; ++$i) {
			$world=DAO::getOne(World::class, mt_rand(1, 10000),false);
			$world->setRandomNumber(mt_rand(1, 10000));
			DAO::update($world);
			$worlds[]=$world->_rest;
		}
		echo json_encode($worlds);
	}

}
