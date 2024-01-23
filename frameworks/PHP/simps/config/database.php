<?php

declare(strict_types=1);

return [
    'host' => 'tfb-database',
    'port' => 3306,
    'database' => 'hello_world',
    'username' => 'benchmarkdbuser',
    'password' => 'benchmarkdbpass',
    'charset' => 'utf8mb4',
    'options' => [
    ],
    'size' => \intdiv(512, swoole_cpu_num()) // 连接池size
];