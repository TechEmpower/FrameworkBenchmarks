<?php
require_once __DIR__.'/db-no-async.php';

use Swoole\Http\Server;
use Swoole\Http\Request;
use Swoole\Http\Response;

$server = new Swoole\Http\Server('0.0.0.0', 8080, SWOOLE_BASE);
$server->set([
    'worker_num' => swoole_cpu_num() * 4,
    'log_file' => '/dev/null',
    'log_level' => 5,
]);

/**
 * On start of the PHP worker. One worker per server process is started.
 */
$server->on('workerStart', function () {
    Db::init();
});

/**
 * On every request to the (web)server, execute the following code
 */
$server->on('request', static function (Request $req, Response $res) {
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
                $res->end(db());
                break;
            
            case '/query':
                $res->header('Content-Type', 'application/json');
                $res->end(query((int) $req->get['q'] ?? 1));
                break;

            case '/fortunes':
                $res->header('Content-Type', 'text/html; charset=utf-8');
                $res->end(fortunes());
                break;

            case '/updates':
                $res->header('Content-Type', 'application/json');
                $res->end(updates((int) $req->get['q'] ?? 1));
                break;

            default:
                $res->status(404);
                $res->end('Not Found.');
        }

    } catch (\Throwable $e) {
        $res->status(500);
        $res->end('code ' . $e->getCode(). 'msg: '. $e->getMessage());
    }
});

$server->start();
