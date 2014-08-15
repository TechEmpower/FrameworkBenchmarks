<?php
if(!defined('INIT')) {	
	define('ROOT',dirname(dirname(dirname(dirname(dirname(__FILE__))))));
	$loader = require_once(ROOT.'/vendor/autoload.php');
	$loader->add('PHPixie', ROOT.'/vendor/phpixie/core/classes/');
	$loader->add('PHPixie', ROOT.'/vendor/phpixie/db/classes/');
	$loader->add('PHPixie',ROOT.'/vendor/phpixie/orm/classes/');
	define('INIT', true);
}
	