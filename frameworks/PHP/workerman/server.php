<?php
require_once __DIR__ . '/vendor/autoload.php';
require_once __DIR__ . '/fortune.php';
require_once __DIR__ . '/dbraw.php';
require_once __DIR__ . '/updateraw.php';
use Workerman\Worker;
use Workerman\Protocols\Http;

$http_worker = new Worker('http://0.0.0.0:8080');
$http_worker->count = (int) shell_exec('nproc') ?? 64;
$http_worker->onWorkerStart = function()
{
  global $pdo;
  $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world;charset=utf8',
  'benchmarkdbuser', 'benchmarkdbpass');
};
$http_worker->onMessage = function($connection, $data)
{
  global $pdo;
  $base = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);

  Http::header('Date: '.gmdate('D, d M Y H:i:s').' GMT');

  if ($base === '/fortune') {
    Http::header('Content-Type: text/html; charset=utf-8');
    ob_start();
    fortune($pdo);
    $connection->send(ob_get_clean());

  } elseif ($base === '/db') {
    Http::header('Content-Type: application/json');
    ob_start();
    dbraw($pdo);
    $connection->send(ob_get_clean());

  } elseif ($base === '/update') {
    Http::header('Content-Type: application/json');
    ob_start();
    updateraw($pdo);
    $connection->send(ob_get_clean());

  } elseif ($base === '/plaintext') {
    Http::header('Content-Type: text/plain');
    $connection->send('Hello, World!');

  } elseif ($base === '/json') {
    Http::header('Content-Type: application/json');
    $connection->send(json_encode(['message'=>'Hello, World!']));
  // } elseif ($base === '/info') {
  //   Http::header('Content-Type: text/plain');
  //   ob_start();
  //   phpinfo();
  //   $connection->send(ob_get_clean());
  }
};

Worker::runAll();
