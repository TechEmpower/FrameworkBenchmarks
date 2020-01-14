<?php

require_once __DIR__ . '/../vendor/autoload.php';

use Illuminate\Database\Capsule\Manager as Capsule;

$capsule = new Capsule;

$capsule->addConnection([
    'driver' => 'mysql',
//    'host' => 'localhost',
    'host' => 'tfb-database',
    'database' => 'hello_world',
    'username' => 'benchmarkdbuser',
    'password' => 'benchmarkdbpass',
    'options'=> [
        PDO::ATTR_PERSISTENT => true,
    ],
]);

$capsule->setAsGlobal();


