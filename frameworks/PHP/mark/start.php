<?php
use Mark\App;
use Workerman\Timer;
use Workerman\Protocols\Http\Response;

require 'vendor/autoload.php';

$api = new App('http://0.0.0.0:8080');

$api->count = (int) shell_exec('nproc');

$api->any('/plaintext', function () {
    global $date;
    return new Response(200, [
        'Content-Type' => 'text/plain',
        'Date'         => $date
    ], 'Hello, World!');
});

$api->get('/json', function () {
    global $date;
    return new Response(200, [
        'Content-Type' => 'application/json',
        'Date'         => $date
    ], \json_encode(['message' => 'Hello, World!']));
});

$date = gmdate('D, d M Y H:i:s').' GMT';

$api->onWorkerStart = function () {
    Timer::add(1, function () {
        global $date;
        $date = gmdate('D, d M Y H:i:s').' GMT';
    });
};

$api->reusePort = true;

$api->start();


