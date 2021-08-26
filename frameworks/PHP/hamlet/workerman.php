<?php

use Benchmark\Application;
use Cache\Adapter\PHPArray\ArrayCachePool;
use Hamlet\Database\PDO\PDODatabase;
use Hamlet\Http\Workerman\Bootstraps\WorkermanBootstrap;

require_once __DIR__ . '/vendor/autoload.php';

$database = new PDODatabase(
    'mysql:host=tfb-database;dbname=hello_world',
    'benchmarkdbuser',
    'benchmarkdbpass'
);
$cache = new ArrayCachePool;
$application = new Application($database, $cache);
WorkermanBootstrap::run('0.0.0.0', 8080, $application);
