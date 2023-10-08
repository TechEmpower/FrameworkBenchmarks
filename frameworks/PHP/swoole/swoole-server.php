<?php
require __DIR__.'/database.php';

use Swoole\Http\Server;
use Swoole\Http\Request;
use Swoole\Http\Response;

$enableCoroutine = getenv('ENABLE_COROUTINE') == 1;
$connection      = $enableCoroutine ? Connections::class : Connection::class;

$server  = new Server('0.0.0.0', 8080);
$setting = [
    'worker_num'        => swoole_cpu_num() * 4,
    'log_file'          => '/dev/null',
    'enable_coroutine'  => $enableCoroutine,
    'enable_reuse_port' => true
];

if ($enableCoroutine) {
    $setting['hook_flags'] = SWOOLE_HOOK_ALL;
    $setting['worker_num'] = swoole_cpu_num();
}

$server->set($setting);


$server->on('workerStart', function () use ($connection) {
    $connection::init(getenv('DATABASE_DRIVER'));
});

$server->on('request', function (Request $req, Response $res) use ($connection) {
    try {
        switch ($req->server['request_uri']) {
            case '/plaintext':
                $res->header['Content-Type'] = 'text/plain; charset=utf-8';
                $res->end('Hello, World!');
                break;
            case '/json':
                $res->header['Content-Type'] = 'application/json';
                $res->end(json_encode(['message' => 'Hello, World!']));
                break;
            case '/db':
                $res->header['Content-Type'] = ['application/json'];
                $res->end($connection::db());
                break;
            case '/fortunes':
                $res->header['Content-Type'] = 'text/html; charset=utf-8';
                $res->end($connection::fortunes());
                break;
            case '/query':
                $res->header['Content-Type'] = 'application/json';
                $res->end($connection::query(
                    isset($req->get['queries']) ? (int) $req->get['queries'] : -1
                ));
                break;
            case '/updates':
                $res->header['Content-Type'] = 'application/json';
                $res->end($connection::updates(
                    isset($req->get['queries']) ? (int) $req->get['queries'] : -1
                ));
                break;

            default:
                $res->status(404);
                $res->end('Error 404');
        }
    } catch (Throwable) {
        $res->status(500);
        $res->end('Error 500');
    }
});

$server->start();
