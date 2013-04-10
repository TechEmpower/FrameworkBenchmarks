<?php
/** @package    HELLO WORLD */

set_include_path('libs/' . PATH_SEPARATOR . get_include_path());

/* require framework libs */
require_once 'verysimple/Phreeze/Dispatcher.php';
require_once 'verysimple/Phreeze/ConnectionSetting.php';
require_once 'verysimple/Phreeze/GenericRouter.php';
require_once 'verysimple/Phreeze/Phreezer.php';

$cs = new ConnectionSetting();
$cs->ConnectionString = "localhost:3306";
$cs->DBName = "hello_world";
$cs->Username = "benchmarkdbuser";
$cs->Password = "benchmarkdbpass";
$cs->Type = "MySQL";

$phreezer = new Phreezer($cs);

$route_map = array(
		'GET:' => array('route' => 'Test.JSON'),
		'GET:json' => array('route' => 'Test.JSON'),
		'GET:db' => array('route' => 'Test.DB')
);

$router = new GenericRouter('./','Test.JSON',$route_map);

Dispatcher::Dispatch(
	$phreezer,
	null,
	'',
	null,
	$router
);
