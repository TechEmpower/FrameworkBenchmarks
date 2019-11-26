<?php
//
// Database Test
//

function dbraw() {
  // Database connection (TODO: When it works, use PDO instead)
  $link = mysql_pconnect('tfb-database', 'benchmarkdbuser', 'benchmarkdbpass');
  mysql_select_db('hello_world', $link);

  // Read number of queries to run from URL parameter
  $query_count = 1;
  $is_multi = isset($_GET['queries']);
  if ($is_multi) {
    $query_count = (int)$_GET['queries'];
    if ($query_count > 500) {
      $query_count = 500;
    } else if ($query_count < 1) {
      $query_count = 1;
    }
  }

  // Create an array with the response string.
  $arr = array();
  $id = mt_rand(1, 10000);

  // For each query, store the result set values in the response array
  while ($query_count--) {
    $result = mysql_query("SELECT id, randomNumber FROM World WHERE id = $id", $link);

    // Store result in array.
    $arr[] = array('id' => $id, 'randomNumber' => mysql_result($result, 0));
    $id = mt_rand(1, 10000);
  }

  mysql_close($link);

  // Set content type
  header("Content-type: application/json");

  // Use the PHP standard JSON encoder.
  // http://www.php.net/manual/en/function.json-encode.php
  if ($is_multi) {
    $output = json_encode($arr);
  } else {
    $output = json_encode($arr[0]);
  }
  // Set content length
  header("Content-Length: {strlen($output)}");

  echo $output;
}

dbraw();
