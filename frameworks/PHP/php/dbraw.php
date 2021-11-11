<?php
header('Content-Type: application/json');

// Database connection
// http://www.php.net/manual/en/ref.pdo-mysql.php
$pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', [
    PDO::ATTR_PERSISTENT => true
]);

// Define query
$statement = $pdo->query( 'SELECT id,randomNumber FROM World WHERE id = '. mt_rand(1, 10000) );

// Use the PHP standard JSON encoder.
// http://www.php.net/manual/en/function.json-encode.php
echo json_encode($statement->fetch(PDO::FETCH_ASSOC), JSON_NUMERIC_CHECK);
