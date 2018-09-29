<?php

use Swoole\Http\Server;

require_once __DIR__ . '/vendor/autoload.php';

\Hamlet\Bootstraps\SwooleBootstrap::run('0.0.0.0', 8080, function () {
    return new \Benchmark\Application;
}, function (Server $server) {
    $server->set([
        'worker_num' => NUMCORES
    ]);
});
