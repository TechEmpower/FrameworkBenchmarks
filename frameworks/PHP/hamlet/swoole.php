<?php

use Benchmark\Application;
use Hamlet\Database\MySQLSwoole\MySQLSwooleDatabase;
use Hamlet\Http\Swoole\Bootstraps\SwooleBootstrap;

require_once __DIR__ . '/vendor/autoload.php';

$database = new MySQLSwooleDatabase(
    'tfb-database',
    'benchmarkdbuser',
    'benchmarkdbpass',
    'hello_world',
    1024
);
$application = new Application($database);
$onWorkerStart = function () use (&$database) {
    $database->warmUp((int) (512 / swoole_cpu_num()));
};
SwooleBootstrap::run('0.0.0.0', 8080, $application, $onWorkerStart);
