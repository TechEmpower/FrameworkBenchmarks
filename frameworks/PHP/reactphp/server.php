<?php

use React\EventLoop\Loop;
use React\Http\HttpServer;
use React\Socket\SocketServer;

require __DIR__ . '/vendor/autoload.php';
require_once __DIR__.'/app.php';

init();

$server = new HttpServer(router(...));
$socket = new SocketServer('0.0.0.0:8080');
$server->listen($socket);

echo "React Server running at http://0.0.0.0:8080\n";
echo "EventLoop: ", Loop::get()::class, "\n";

