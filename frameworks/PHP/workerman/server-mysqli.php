<?php
require_once __DIR__.'/vendor/autoload.php';
require_once __DIR__.'/db-mysqli.php';

use Workerman\Protocols\Http;
use Workerman\Worker;

$http_worker                = new Worker('http://0.0.0.0:8080');
$http_worker->count         = shell_exec('nproc');
$http_worker->onWorkerStart = function () {
    global $db;
    $db = new mysqli('p:tfb-database', 'benchmarkdbuser', 'benchmarkdbpass', 'hello_world');
};

$http_worker->onMessage = static function ($connection) {

    Http::header('Date: '.gmdate('D, d M Y H:i:s').' GMT');

    switch (parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH)) {

        case '/db':
            Http::header('Content-Type: application/json');
            return $connection->send(db());

        case '/fortune':
            //Http::header('Content-Type: text/html; charset=utf-8');
            return $connection->send(fortune());

        case '/update':
            Http::header('Content-Type: application/json');
            return $connection->send(update());

        //case '/info':
        //   Http::header('Content-Type: text/plain');
        //   ob_start();
        //   phpinfo();
        //   return $connection->send(ob_get_clean());

        default:
            Http::responseCode(404);
            $connection->send('Error 404');
    }
};

Worker::runAll();
