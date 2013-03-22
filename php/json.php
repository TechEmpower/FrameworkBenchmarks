<?php
//
// JSON Encoding Test
//

// Set content type
header("Content-type: application/json");

// Create an array with the response string.
$arr = array(
    "message" => "Hello, World!"
);

// Use the PHP standard JSON encoder.
// http://www.php.net/manual/en/function.json-encode.php
echo json_encode($arr);
?>
