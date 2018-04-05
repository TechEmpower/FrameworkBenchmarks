<?php
require_once __DIR__ . '/vendor/autoload.php';
require_once __DIR__ . '/fortune.php';
require_once __DIR__ . '/dbraw.php';
require_once __DIR__ . '/updateraw.php';
use Workerman\Worker;
use Workerman\Protocols\Http;

function get_processor_cores_number() {
  $command = 'cat /proc/cpuinfo | grep processor | wc -l';
  return  (int) shell_exec($command);
}

$http_worker = new Worker('http://0.0.0.0:8080');
$http_worker->count = get_processor_cores_number() * 2 || 8;
$http_worker->onMessage = function($connection, $data)
{
  $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world;charset=utf8',
  'benchmarkdbuser', 'benchmarkdbpass', array(
    PDO::ATTR_PERSISTENT => true
  ));
  $base = $_SERVER['REQUEST_URI'];
  $question = strpos($base, '?');
  if ($question !== false) {
    $base = substr($base, 0, $question);
  }
  Http::header('Date: '.gmdate('D, d M Y H:i:s', time()).' GMT'); 
  if ($base == '/fortune.php') {
    Http::header('Content-Type: text/html; charset=utf-8');
    ob_start();
    fortune($pdo);
    $connection->send(ob_get_clean());
  } else if ($base == '/dbraw.php') {
    Http::header('Content-Type: application/json');
    ob_start();
    dbraw($pdo);
    $connection->send(ob_get_clean());
  } else if ($base == '/updateraw.php') {
    Http::header('Content-Type: application/json');
    ob_start();
    updateraw($pdo);
    $connection->send(ob_get_clean());
  } else if ($base == '/plaintext.php') {
    Http::header('Content-Type: text/plain');
    $connection->send('Hello, World!');
  } else if ($base == '/json.php') {
    Http::header('Content-Type: application/json');
    $connection->send(json_encode(['message'=>'Hello, World!']));
  } else {
    Http::header('Content-Type: application/json');
    $connection->send(json_encode(['message'=>'Hello, World!']));
  }
};

Worker::runAll();
