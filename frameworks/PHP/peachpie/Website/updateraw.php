<?php
//
// Database Test
//

function updateraw() {
  // Database connection (TODO: When it works, use PDO instead)
  $link = mysql_connect('tfb-database', 'benchmarkdbuser', 'benchmarkdbpass');
  mysql_select_db('hello_world', $link);

  // Read number of queries to run from URL parameter
  $query_count = 1;
  if (isset($_GET['queries'])) {
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
  $randomNumber = mt_rand(1, 1000);

  // For each query, store the result set values in the response array
  while (0 < $query_count--) {
    $result = mysql_query("SELECT randomNumber FROM World WHERE id = $id", $link);
    
    // Store result in array.
    $world = array('id' => $id, 'randomNumber' => mysql_result($result, 0));
    $world['randomNumber'] = $randomNumber;

    mysql_query("UPDATE World SET randomNumber = $randomNumber WHERE id = $id", $link);
    
    $arr[] = $world;
    $id = mt_rand(1, 10000);
    $randomNumber = mt_rand(1, 10000);
  }

  mysql_close($link);

  // Set content type
  header("Content-type: application/json");

  // Use the PHP standard JSON encoder.
  // http://www.php.net/manual/en/function.json-encode.php
  $output = json_encode($arr);
  // Set content length
  header("Content-Length: {strlen($output)}");

  echo $output;
}

updateraw();
?>
