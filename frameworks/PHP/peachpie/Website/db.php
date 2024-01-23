<?php
//
// Database Test
//

function db() {
  
  // Set content type
  header("Content-type: application/json");

  // Database connection (TODO: When it works, use PDO instead)
  $link = mysql_pconnect('tfb-database', 'benchmarkdbuser', 'benchmarkdbpass');
  mysql_select_db('hello_world', $link);

  $id = mt_rand(1, 10000);
  $result = mysql_query("SELECT id,randomNumber FROM World WHERE id=$id", $link);

  // Store result in array.
  echo json_encode( mysql_fetch_assoc($result) );

  mysql_close($link);
}

db();
