<?php

return [
    'default' => env('MYSQL_DEFAULT', 'default'),
    'connections' => [
        env('MYSQL_DEFAULT', 'default') => [
            'dsn' => env('MYSQL_DSN', 'mysql:dbname=sw_test;host=127.0.0.1'),
            'username' => env('MYSQL_USERNAME', 'root'),
            'passwd' => env('MYSQL_PASSWD', null),
            'options' => [
                \PDO::ATTR_CASE => \PDO::CASE_NATURAL,
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
                \PDO::ATTR_ORACLE_NULLS => \PDO::NULL_NATURAL,
                \PDO::ATTR_STRINGIFY_FETCHES => false,
                \PDO::ATTR_EMULATE_PREPARES => false,
            ],
            'pool_size' => envInt('MYSQL_POOL_SIZE', 5),
        ],
    ],
    'switch' => envInt('MYSQL_SWITCH', 0),
    'pool_change_event' => envInt('MYSQL_POOL_CHANGE_EVENT', 0),
    'report_pool_change' => envInt('MYSQL_REPORT_POOL_CHANGE', 0),
];
