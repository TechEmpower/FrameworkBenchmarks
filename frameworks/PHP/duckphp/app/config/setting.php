<?php declare(strict_types=1);
/**
 * DuckPHP
 * From this time, you never be alone~
 */

return [
    'duckphp_is_debug' => true, // turn on ,for test er ,don't turn on the debug again
    'duckphp_platform' => 'default',
    'database' => [
        //'dsn' => 'mysql:host=172.19.0.3;dbname=hello_world;',
        //'dsn' => 'mysql:host=127.0.0.1;dbname=hello_world;',
        'dsn' => 'mysql:host=tfb-database;dbname=hello_world;',
        'username' => 'benchmarkdbuser',
        'password' => 'benchmarkdbpass',
        'driver_options' => [ 
            PDO::ATTR_PERSISTENT => true
        ],
    ],
];
