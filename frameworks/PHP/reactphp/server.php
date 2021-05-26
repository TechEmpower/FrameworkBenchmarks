<?php

require __DIR__ . '/vendor/autoload.php';
require_once __DIR__.'/app.php';

init();

$loop = React\EventLoop\Factory::create();

$server = new React\Http\Server($loop, function (Psr\Http\Message\ServerRequestInterface $request) {
    return router($request);
});

$socket = new React\Socket\Server('0.0.0.0:8080', $loop);
$server->listen($socket);

echo "React Server running at http://0.0.0.0:8080\n";

$loop->run();
