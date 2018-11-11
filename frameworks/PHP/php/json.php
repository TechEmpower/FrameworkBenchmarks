<?php
// Set content type
header('Content-type: application/json');

// Use the PHP standard JSON encoder.
// http://www.php.net/manual/en/function.json-encode.php
echo json_encode(array('message' => 'Hello, World!'));
