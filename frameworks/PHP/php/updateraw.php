<?php
//
// Database Test
//

// Database connection
// http://www.php.net/manual/en/ref.pdo-mysql.php
$pdo = new PDO('mysql:host=localhost;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
    PDO::ATTR_PERSISTENT => true
));

// Read number of queries to run from URL parameter
$query_count = 1;
if (TRUE === isset($_GET['queries'])) {
  $query_count = $_GET['queries'];
}

// Create an array with the response string.
$arr = array();
$id = mt_rand(1, 10000);
$randomNumber = mt_rand(1, 1000);

// Define query
$statement = $pdo->prepare('SELECT randomNumber FROM World WHERE id = :id');
$statement->bindParam(':id', $id, PDO::PARAM_INT);

$updateStatement = $pdo->prepare('UPDATE World SET randomNumber = :randomNumber WHERE id = :id');
$updateStatement->bindParam(':id', $id, PDO::PARAM_INT);
$updateStatement->bindParam(':randomNumber', $randomNumber, PDO::PARAM_INT);

// For each query, store the result set values in the response array
while (0 < $query_count--) {
  $statement->execute();
  
  // Store result in array.
  $world = array('id' => $id, 'randomNumber' => $statement->fetchColumn());
  $world['randomNumber'] = $randomNumber;
  $updateStatement->execute();
  
  $arr[] = $world;
  $id = mt_rand(1, 10000);
  $randomNumber = mt_rand(1, 10000);
}

// Use the PHP standard JSON encoder.
// http://www.php.net/manual/en/function.json-encode.php
echo json_encode($arr);
?>
