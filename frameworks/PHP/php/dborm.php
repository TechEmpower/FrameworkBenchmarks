<?php
//
// Database Test
//

// Set content type
header("Content-type: application/json");

// Database connection
// http://www.php.net/manual/en/ref.pdo-mysql.php
// $pdo = new PDO('mysql:host=localhost;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass');

# inclue the ActiveRecord library
require_once 'php-activerecord/ActiveRecord.php';

ActiveRecord\Config::initialize(function($cfg)
{
  $cfg->set_model_directory('models');
  $cfg->set_connections(array('development' =>
    'mysql://benchmarkdbuser:benchmarkdbpass@localhost/hello_world'));
});

// Read number of queries to run from URL parameter
$query_count = 1;
if (!empty($_GET)) {
  $query_count = $_GET["queries"];
}

// Create an array with the response string.
$arr = array();

// For each query, store the result set values in the response array
for ($i = 0; $i < $query_count; $i++) {
  // Choose a random row
  // http://www.php.net/mt_rand
  $id = mt_rand(1, 10000);

  $world = World::find_by_id($id);
  
  // Store result in array.
  $arr[] = $world->to_json();
}

// Use the PHP standard JSON encoder.
// http://www.php.net/manual/en/function.json-encode.php
echo json_encode($arr);
?>
