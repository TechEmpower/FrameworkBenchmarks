<?php

require_once __DIR__ . '/vendor/autoload.php';

$database = new \Hamlet\Database\MySQLSwoole\MySQLSwooleDatabase(
    'tfb-database',
    'benchmarkdbuser',
    'benchmarkdbpass',
    'hello_world'
);
$application = new \Benchmark\Application($database);
\Hamlet\Http\Swoole\Bootstraps\SwooleBootstrap::run('0.0.0.0', 8080, $application);
