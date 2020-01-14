<?php
// Set content type
header('Content-Type: application/json');

// Database connection
// http://www.php.net/manual/en/ref.pdo-mysql.php
// $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass');

# inclue the ActiveRecord library
require 'vendor/php-activerecord/php-activerecord/ActiveRecord.php';

ActiveRecord\Connection::$PDO_OPTIONS[PDO::ATTR_PERSISTENT] = true;
ActiveRecord\Config::initialize(function ($cfg) {
    $cfg->set_model_directory('models');
    $cfg->set_connections(['development' =>
        'mysql://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world']);
});

if (! isset($_GET['queries'])) {
    echo json_encode( World::find_by_id(mt_rand(1, 10000))->to_array(), JSON_NUMERIC_CHECK);
    return;
}

// Read number of queries to run from URL parameter
$query_count = 1;
if ($_GET['queries'] > 1) {
    $query_count = $_GET['queries'] > 500 ? 500 : $_GET['queries'];
}
// Create an array with the response string.
$arr = [];
// For each query, store the result set values in the response array
while ($query_count--) {
    // Store result in array.
    $arr[] = World::find_by_id(mt_rand(1, 10000))->to_array();
}

// Use the PHP standard JSON encoder.
// http://www.php.net/manual/en/function.json-encode.php
echo json_encode($arr, JSON_NUMERIC_CHECK);
