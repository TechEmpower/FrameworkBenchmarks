<?php

require_once __DIR__ . '/vendor/autoload.php';

$database = new \Hamlet\Database\MySQL\MySQLDatabase(
    'p:tfb-database',
    'benchmarkdbuser',
    'benchmarkdbpass',
    'hello_world'
);
$application = new \Benchmark\Application($database);
\Hamlet\Http\Bootstraps\ServerBootstrap::run($application);
