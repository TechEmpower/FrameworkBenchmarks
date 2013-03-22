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
if (!empty($_GET)) {
  $query_count = $_GET["queries"];
}

// Create an array with the response string.
$arr = array();

// Define query
$statement = $pdo->prepare("SELECT * FROM World WHERE id = :id");

// For each query, store the result set values in the response array
for ($i = 0; $i < $query_count; $i++) {
  // Choose a random row
  // http://www.php.net/mt_rand
  $id = mt_rand(1, 10000);

  // Bind id to query
  $statement->bindValue(':id', $id, PDO::PARAM_INT);
  $statement->execute();
  $row = $statement->fetch(PDO::FETCH_ASSOC);
  
  // Store result in array.
  $arr[] = array("id" => $id, "randomNumber" => $row['randomNumber']);
}

// Use the PHP standard JSON encoder.
// http://www.php.net/manual/en/function.json-encode.php
echo json_encode($arr);
?>
