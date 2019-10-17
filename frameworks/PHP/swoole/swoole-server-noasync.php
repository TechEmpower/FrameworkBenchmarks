<?php
require_once __DIR__.'/db-no-async.php';

use Swoole\Http\Request;
use Swoole\Http\Response;

$server = new swoole_http_server('0.0.0.0', 8080, SWOOLE_BASE);
$server->set([
    'worker_num' => swoole_cpu_num()
]);

/**
 * On start of the PHP worker. One worker per server process is started.
 */
$server->on('workerStart', function () {
    global $pdo;
    $pdo = new PDO("mysql:host=tfb-database;dbname=hello_world", "benchmarkdbuser", "benchmarkdbpass", [
        PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION
    ]);
});

/**
 * On every request to the (web)server, execute the following code
 */
$server->on('request', function (Request $req, Response $res) use ($db, $fortunes, $updates) {
    try {
        switch ($req->server['request_uri']) {
            case '/json':
                $res->header('Content-Type', 'application/json');
                $res->end(json_encode(['message' => 'Hello, World!']));
                break;

            case '/plaintext':
                $res->header('Content-Type', 'text/plain; charset=utf-8');
                $res->end('Hello, World!');
                break;

            case '/db':
                $res->header('Content-Type', 'application/json');

                if (isset($req->get['queries'])) {
                    $res->end(db((int) $req->get['queries']));
                } else {
                    $res->end(db(-1));
                }
                break; 

            case '/fortunes':
                $res->header('Content-Type', 'text/html; charset=utf-8');
                $res->end(fortunes());
                break;

            case '/updates':
                $res->header('Content-Type', 'application/json');
                $res->end(updates((int) $req->get['queries'] ?? 1));
                break;
        }

    } catch (\Throwable $e) {
        $res->status(500);
        $res->end('code ' . $e->getCode(). 'msg: '. $e->getMessage());
    }
});

$server->start();
