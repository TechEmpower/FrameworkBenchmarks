<?php

error_reporting(-1);

require_once __DIR__ . '/../vendor/silica/silica/src/Silica/Application.php' ;

$app = new Silica\Application();

$app
->share('pdo', function($app) {
    $pdo    = new PDO('mysql::host=192.168.100.102;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
				PDO::ATTR_PERSISTENT => true ,
           ) ) ;
    return $pdo ;
})
>get('/json', function() {
	echo json_encode(array("message" => "Hello World!"));
}) 
>get('/db', function() use ($app) {
	
	$query_count = 1;
	if (TRUE === isset($_GET['queries'])) {
	  $query_count = $_GET['queries'];
	}

	// Create an array with the response string.
	$arr = array();
	$id = mt_rand(1, 10000);

	// Define query
	$statement = $app['pdo']->prepare('SELECT randomNumber FROM World WHERE id = :id');
	$statement->bindParam(':id', $id, PDO::PARAM_INT);

	// For each query, store the result set values in the response array
	while (0 < $query_count--) {
	  $statement->execute();
  
	  // Store result in array.
	  $arr[] = array('id' => $id, 'randomNumber' => $statement->fetchColumn());
	  $id = mt_rand(1, 10000);
	}

	// Use the PHP standard JSON encoder.
	// http://www.php.net/manual/en/function.json-encode.php
	echo json_encode($arr);
})
>run() ;




