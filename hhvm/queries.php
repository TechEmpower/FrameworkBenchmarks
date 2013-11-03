<?php
//
// 3. Multiple database queries
//

// Database connection
$pdo = new PDO('mysql:host=localhost;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
    PDO::ATTR_PERSISTENT => true
));

// Read number of queries to run from URL parameter
$query_count = 1;
if (!empty($_GET['queries'])) {
  $query_count = intval($_GET['queries']);
}

// Fix the queries limits
$query_count = $query_count < 1 ? 1 : ($query_count > 500 ? 500 : $query_count);

// Create an array with the response string.
$arr = array();
$id = mt_rand(1, 10000);

// Define query
$statement = $pdo->prepare('SELECT randomNumber FROM World WHERE id = :id');
$statement->bindParam(':id', $id, PDO::PARAM_INT);

// For each query, store the result set values in the response array
while (0 < $query_count--) {
  $statement->execute();

  // Store result in array.
  $arr[] = array('id' => $id, 'randomNumber' => $statement->fetchColumn());
  $id = mt_rand(1, 10000);
}

// Send the required parameters
header('Content-Type: application/json');
echo json_encode($arr);
