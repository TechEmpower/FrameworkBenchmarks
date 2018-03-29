<?php
header('Content-type: application/json');

// Database connection
// http://www.php.net/manual/en/ref.pdo-mysql.php
$pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
    PDO::ATTR_PERSISTENT => true
));

// Read number of queries to run from URL parameter
$query_count = 1;
if (isset($_GET['queries']) && $_GET['queries'] > 0) {
  $query_count = $_GET['queries'] > 500 ? 500 : $_GET['queries'];
}

// Create an array with the response string.
$arr = array();

// Define query
$statement = $pdo->prepare('SELECT randomNumber FROM World WHERE id = ?');

// For each query, store the result set values in the response array
while (0 < $query_count--) {
  $id = mt_rand(1, 10000);
  $statement->execute(array($id));
  
  // Store result in array.
  $arr[] = array('id' => $id, 'randomNumber' => $statement->fetchColumn());
}

// Use the PHP standard JSON encoder.
// http://www.php.net/manual/en/function.json-encode.php
if (count($arr) === 1) {
      $arr = $arr[0];
}

echo json_encode($arr);
