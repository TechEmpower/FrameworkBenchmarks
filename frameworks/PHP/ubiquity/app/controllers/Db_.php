<?php
namespace controllers;

use Ubiquity\controllers\Startup;
use Ubiquity\utils\http\UResponse;


/**
 * Bench controller.
 **/
class Db_ extends Db{
	public function initialize(){
		UResponse::setContentType( 'application/json' );
		$config=Startup::getConfig();
		\Ubiquity\orm\DAO::startDatabase($config);
	}
}
