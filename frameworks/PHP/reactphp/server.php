<?php

use React\EventLoop\Loop;
use React\Http\HttpServer;
use React\Socket\SocketServer;

require __DIR__ . '/vendor/autoload.php';
require_once __DIR__.'/app.php';

$server = new HttpServer(requestHandler());
$socket = new SocketServer('0.0.0.0:8080');
$server->listen($socket);

echo "React Server running at http://0.0.0.0:8080\n";
echo "EventLoop: ", Loop::get()::class, "\n";

$interrupt = static function () use ($server, $socket, &$interrupt): void {
    echo 'Interrupting server', PHP_EOL;

    $socket->close();

    Loop::removeSignal(SIGINT, $interrupt);
    Loop::removeSignal(SIGTERM, $interrupt);
};

Loop::addSignal(SIGINT, $interrupt);
Loop::addSignal(SIGTERM, $interrupt);
