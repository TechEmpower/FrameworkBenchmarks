<?php
require_once __DIR__.'/vendor/autoload.php';
require_once __DIR__.'/fortune.php';
require_once __DIR__.'/dbraw.php';
require_once __DIR__.'/updateraw.php';
use Workerman\Protocols\Http;
use Workerman\Worker;

$http_worker                = new Worker('http://0.0.0.0:8080');
$http_worker->count         = (int) shell_exec('nproc') ?? 64;
$http_worker->onWorkerStart = function () {
    global $pdo;
    $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world',
        'benchmarkdbuser', 'benchmarkdbpass');
};
$http_worker->onMessage = static function ($connection) {

    Http::header('Date: '.gmdate('D, d M Y H:i:s').' GMT');

    switch (parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH)) {
        case '/plaintext':
            Http::header('Content-Type: text/plain');
            return $connection->send('Hello, World!');

        case '/json':
            Http::header('Content-Type: application/json');
            return $connection->send(json_encode(['message' => 'Hello, World!']));

        case '/db':
            Http::header('Content-Type: application/json');
            return $connection->send(dbraw());

        case '/fortune':
            Http::header('Content-Type: text/html; charset=utf-8');
            return $connection->send(fortune());

        case '/update':
            Http::header('Content-Type: application/json');
            return $connection->send(updateraw());

            //case '/info':
            //   Http::header('Content-Type: text/plain');
            //   ob_start();
            //   phpinfo();
            //   $connection->send(ob_get_clean());

            //default:
            //   $connection->send('error');
    }
};

Worker::runAll();
