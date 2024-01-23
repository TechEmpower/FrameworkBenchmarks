<?php

require_once __DIR__ . '/boot-eloquent.php';

// Read number of queries to run from URL parameter
$query_count = 1;
if (isset($_GET['queries']) && (int) $_GET['queries'] > 0) {
  $query_count = $_GET['queries'] > 500 ? 500 : $_GET['queries'];
}

// Create an array with the response string.
$arr = array();

// For each query, store the result set values in the response array
while ($query_count--) {
  $id = mt_rand(1, 10000);
  $randomNumber = mt_rand(1, 10000);

  $world = World::find($id);
  $world->randomNumber = $randomNumber;
  $world->save();

  $arr[] = $world->toArray();
}

// Use the PHP standard JSON encoder.
// http://www.php.net/manual/en/function.json-encode.php
header('Content-Type: application/json');
echo json_encode($arr);
