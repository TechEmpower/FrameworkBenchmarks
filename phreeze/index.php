<?php
/** @package    HELLO WORLD */

set_include_path('libs/');

/* require framework libs */
require_once 'verysimple/Phreeze/Dispatcher.php';
require_once 'verysimple/Phreeze/ConnectionSetting.php';
require_once 'verysimple/Phreeze/SimpleRouter.php';
require_once 'verysimple/Phreeze/Phreezer.php';
require_once 'Controller/TestController.php';

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
		'GET:db' => array('route' => 'Test.DB'),
		'GET:fortunes' => array('route' => 'Test.Fortunes'),
		'GET:updates' => array('route' => 'Test.Updates'),
		'GET:plaintext' => array('route' => 'Test.PlainText')
);

$router = new SimpleRouter('/','Test.JSON',$route_map);

Dispatcher::Dispatch(
	$phreezer,
	null,
	'',
	null,
	$router
);
