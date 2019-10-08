<?php
require_once __DIR__.'/vendor/autoload.php';
require_once __DIR__.'/fortune.php';
require_once __DIR__.'/dbraw.php';
require_once __DIR__.'/updateraw.php';
use Workerman\Protocols\Http;
use Workerman\Worker;

$http_worker                = new Worker('http://0.0.0.0:8080');
$http_worker->count         = shell_exec('nproc') * 3;
$http_worker->onWorkerStart = function () {
    global $pdo, $fortune, $statement;
    $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world',
        'benchmarkdbuser', 'benchmarkdbpass',
        [PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC]);
    $fortune   = $pdo->prepare('SELECT id,message FROM Fortune');
    $statement = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
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
            //   Http::header('HTTP', true, 404);
            //   $connection->send('Error 404');
    }
};

Worker::runAll();
