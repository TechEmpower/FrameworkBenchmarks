<?php
// Set content type
header('Content-Type: application/json');

// Use the PHP standard JSON encoder.
// http://www.php.net/manual/en/function.json-encode.php
echo json_encode(['message' => 'Hello, World!']);
