<?php

error_reporting(-1);

require_once __DIR__ . '/../vendor/silica/silica/src/Silica/Application.php' ;

$app = new Silica\Application();

$app->share('pdo', function($app) {
    $options = array(
                // PDO::MYSQL_ATTR_INIT_COMMAND => "SET NAMES utf8",
                PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION,
                PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_OBJ ,
           );
    $pdo    = new PDO('mysql::host=192.168.100.102;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', $options ) ;
    return $pdo ;
}) ;

$app->get('/json', function() {
	echo json_encode(array("message" => "Hello World!"));
}) ;

$app->get('/db', function() use ($app) {
    $queries =  1 ;
	if( isset($_GET['queries']) ) {
		$queries = (int) $_GET['queries'] ;
	}
    // possibility for micro enhancement could be the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
    $worlds = array();

    for($i = 0; $i < $queries; ++$i) {
        $worlds[] = $app['db']->fetchAssoc('SELECT * FROM World WHERE id = ?', array(mt_rand(1, 10000)));
    }
	echo json_encode( $worlds ) ;
});


$app->run();




