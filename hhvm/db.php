<?php
//
// 2. Single database query
//

// Database connection
$pdo = new PDO('mysql:host=localhost;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
    PDO::ATTR_PERSISTENT => true
));

// Create an array with the response string.
$arr = array();
$id = mt_rand(1, 10000);

// Define query
$statement = $pdo->prepare('SELECT randomNumber FROM World WHERE id = :id');
$statement->bindParam(':id', $id, PDO::PARAM_INT);
$statement->execute();

// Store result in array.
$arr = array('id' => $id, 'randomNumber' => $statement->fetchColumn());
$id = mt_rand(1, 10000);

// Send the required parameters
header('Content-Type: application/json');
echo json_encode($arr);

