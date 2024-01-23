<?php

use Benchmark\Application;
use Hamlet\Database\PDO\PDODatabase;
use Hamlet\Http\Bootstraps\ServerBootstrap;

require_once __DIR__ . '/vendor/autoload.php';

$database = new PDODatabase(
    'mysql:host=tfb-database;dbname=hello_world',
    'benchmarkdbuser',
    'benchmarkdbpass'
);
$application = new Application($database);
ServerBootstrap::run($application);
