<?php
header('Content-Type: application/json');

// Database connection
// http://www.php.net/manual/en/ref.pdo-mysql.php
$pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', [
    PDO::ATTR_PERSISTENT => true
]);

// Read number of queries to run from URL parameter
$query_count = 1;
if ((int) $_GET['queries'] > 1) {
  $query_count = min($_GET['queries'], 500);
}

// Define query
$statement = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id = ?');

// For each query, store the result set values in the response array
while ($query_count--) {
  $statement->execute( [mt_rand(1, 10000)] );
  
  // Store result in array.
  $arr[] = $statement->fetch(PDO::FETCH_ASSOC);
}

// Use the PHP standard JSON encoder.
// http://www.php.net/manual/en/function.json-encode.php
echo json_encode($arr, JSON_NUMERIC_CHECK);
