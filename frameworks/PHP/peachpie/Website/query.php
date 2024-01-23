<?php
//
// Database Test
//

function query() {
  
  // Set content type
  header("Content-type: application/json");
  
  // Database connection (TODO: When it works, use PDO instead)
  $link = mysql_pconnect('tfb-database', 'benchmarkdbuser', 'benchmarkdbpass');
  mysql_select_db('hello_world', $link);

  // Read number of queries to run from URL parameter
  $query_count = max(1, min(500, (int)$_GET['queries']));

  // Create an array with the response string.
  $arr = [];
  
  // For each query, store the result set values in the response array
  while ($query_count--) {
    $id = mt_rand(1, 10000);
    $result = mysql_query("SELECT id,randomNumber FROM World WHERE id=$id", $link);

    // Store result in array.
    $arr[] = mysql_fetch_assoc($result);
  }

  mysql_close($link);

  // Use the PHP standard JSON encoder.
  // http://www.php.net/manual/en/function.json-encode.php
  echo json_encode($arr);
}

query();
