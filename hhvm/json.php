<?php
//
// 1. JSON Test
//

function main() {
  $data = json_encode(array('message' => 'Hello, World!'));
  header('Content-Type: application/json');
  echo $data;
}
main();
