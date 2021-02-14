<?php

return [
    'debug_log' => false, // 是否打印sql日志
    'default'      => [
        'max_connect_count' => 6,
        'dns'               => env('mysql.default.dns', 'mysql:host=127.0.0.1;dbname=test'),
        'username'          => env('mysql.default.username', 'root'),
        'password'          => env('mysql.default.password', '123456'),
        'ops'               => [
            PDO::MYSQL_ATTR_INIT_COMMAND => 'SET NAMES utf8mb4',
            PDO::ATTR_EMULATE_PREPARES   => false,
            PDO::ATTR_PERSISTENT         => false
        ]
    ]
];
