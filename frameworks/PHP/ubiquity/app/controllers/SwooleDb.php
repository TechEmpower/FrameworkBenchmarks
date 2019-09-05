<?php
namespace controllers;

use Ubiquity\orm\DAO;
use models\World;
use Swoole\Coroutine as co;

/**
 * Bench controller.
 */
class SwooleDb extends \Ubiquity\controllers\Controller {

	public function initialize() {
		\Ubiquity\utils\http\UResponse::setContentType('application/json');
	}
	
	public function index() {
	    $dbInstance=DAO::pool();
		$world = DAO::getById(World::class, \mt_rand(1, 10000), false);
		DAO::freePool($dbInstance);
		echo \json_encode($world->_rest);
	}
	
	public function query($queries = 1) {
		$worlds = [];
		$queries = \min(\max($queries, 1), 500);
		$dbInstance=DAO::pool();
		for ($i = 0; $i < $queries; ++ $i) {
			$worlds[] = (DAO::getById(World::class, \mt_rand(1, 10000), false))->_rest;
		}
		DAO::freePool($dbInstance);
		echo \json_encode($worlds);
	}
	
	public function update($queries = 1) {
	    \Swoole\Runtime::enableCoroutine();
	    $queries = \min(\max($queries, 1), 500);
	    $worlds = new co\Channel($queries);
	    $count=\min(5,$queries);
	    $rest=$queries%$count;
	    $nb=($queries-$rest)/$count;
	    for ($i = 0; $i < $nb; ++ $i) {
	        $this->_update($count, $worlds);
	    }
	    if($rest>0){
	        $this->_update($rest, $worlds);
	    }
	    $result=[];
	    for($i=0;$i<$queries;++$i){
	        $result[]=$worlds->pop();
	    }
	    echo \json_encode($result);
	}
	
	private function _update($queries,$worlds) {
        go(static function() use($queries,$worlds){
	         $dbInstance=DAO::pool();
	         for ($j = 0; $j < $queries; ++ $j) {
	             $world = DAO::getById(World::class, \mt_rand(1, 10000), false);
	             $world->randomNumber = \mt_rand(1, 10000);
	             DAO::update($world);
	             $worlds->push($world->_rest);
	         }
	         DAO::freePool($dbInstance);
        });
	}
}
