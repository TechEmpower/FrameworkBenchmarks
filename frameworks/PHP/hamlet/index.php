<?php

use Benchmark\Application;
use Hamlet\Database\MySQL\MySQLDatabase;
use Hamlet\Http\Bootstraps\ServerBootstrap;

require_once __DIR__ . '/vendor/autoload.php';

$database = new MySQLDatabase(
    'p:tfb-database',
    'benchmarkdbuser',
    'benchmarkdbpass',
    'hello_world'
);
$application = new Application($database);
ServerBootstrap::run($application);
