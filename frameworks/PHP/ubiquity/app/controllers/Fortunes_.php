<?php

namespace controllers;

use Ubiquity\controllers\Startup;
use Ubiquity\views\engine\micro\MicroTemplateEngine;

class Fortunes_ extends Fortunes {
	public function initialize(){
		Startup::$templateEngine=new MicroTemplateEngine();
		$config=Startup::getConfig();
		\Ubiquity\orm\DAO::startDatabase($config);
	}
}

