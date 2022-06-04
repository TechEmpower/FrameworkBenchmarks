<?php
//
// Database Test
//

// Set content type
header("Content-type: application/json");

function updateraw(int $query_count) {
  // Database connection // TODO: use PDO instead
  $link = mysql_pconnect('tfb-database', 'benchmarkdbuser', 'benchmarkdbpass');
  mysql_select_db('hello_world', $link);

  // Create an array with the response string.
  $arr = [];

  // Ensure the number of queries to run from URL parameter
  $query_count = max(min($query_count, 500), 1);

  // For each query, store the result set values in the response array
  while ($query_count--) {

    $id = mt_rand(1, 10000);
    $randomNumber = mt_rand(1, 10000);

    $result = mysql_query("SELECT id, randomNumber FROM World WHERE id = $id", $link);

    // Store result in array.
    $world = ['id' => $id, 'randomNumber' => mysql_result($result, 0)];
    $world['randomNumber'] = $randomNumber;

    mysql_query("UPDATE World SET randomNumber = $randomNumber WHERE id = $id", $link);

    $arr[] = $world;
  }

  mysql_close($link);

  // Use the PHP standard JSON encoder.
  // http://www.php.net/manual/en/function.json-encode.php
  echo json_encode($arr);
}

updateraw((int)$_GET['queries']);
