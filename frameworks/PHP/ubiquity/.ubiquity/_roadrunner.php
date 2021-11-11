<?php
use RoadRunnerUbiquity\Request;
use Ubiquity\controllers\StartupAsync;

define ( 'DS', DIRECTORY_SEPARATOR );
define ( 'ROOT', __DIR__ . DS . '..' . DS . 'app' . DS );

ini_set ( 'display_errors', 'stderr' );
ini_set ( 'max_execution_time', 0 );

$config = include_once ROOT . 'config/config.php';
$config ['siteUrl'] = 'http://127.0.0.1:8080/';

require_once ROOT . '../vendor/autoload.php';
require_once ROOT . 'config/rrServices.php';

$request = new Request ( $config );
StartupAsync::init ( $config );

while ( $request->acceptRequest () ) {
	StartupAsync::forward ( $request->ubiquityRoute () );
	$request->sendResponse ()->garbageCollect ();
}
