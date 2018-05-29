<?php
//
// JSON Encoding Test
//

function json() {
  // Set content type
  header("Content-type: application/json");

  // Create an array with the response string.
  $arr = array(
      "message" => "Hello, World!"
  );

  // Use the PHP standard JSON encoder.
  // http://www.php.net/manual/en/function.json-encode.php
  $output =  json_encode($arr);

  // Set content length
  header("Content-Length: {strlen($output)}");

  echo $output;
}

json();
?>
