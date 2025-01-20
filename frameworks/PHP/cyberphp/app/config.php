<?php
return [
    'app_name' => 'Cyber',
    // Request middleware runs after obtaining request body and before parsing route
    // Mainly used for blacklist, whitelist, system maintenance, request filtering, data access, etc.
    'request_middleware' => [
        // \app\common\middleware\IpBlacklistMiddleware::class,// IP blacklist middleware
        // \app\middleware\RateLimitMiddleware::class,// Rate limit middleware
        // \app\middleware\SecurityMiddleware::class, // Security protection (CSRF/XSS filtering/SQL injection) middleware
    ],
    // Business middleware runs after parsing route and before executing controller method
    // Mainly used for common business such as user authentication
    'middleware' => [
        // \app\common\middleware\Route1Middleware::class,
        // \app\common\middleware\Route2Middleware::class,
    ],
    'orm' => 'pdo',
    'pdo' => [
        'dsn' => 'pgsql:host=tfb-database;dbname=hello_world',
        'username' => 'benchmarkdbuser',
        'password' => 'benchmarkdbpass',
        'options' => [PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,PDO::ATTR_EMULATE_PREPARES    => false]
    ],
    'eloquent' => [
        'driver' => 'mysql',
        'host' => '127.0.0.1',
        'database' => 'lavaman',
        'username' => 'root',
        'password' => 'root',
        'charset' => 'utf8mb4',
        'prefix' => '',
    ],
    'thinkorm' => [
        'default'    =>    'mysql',
        'connections'    =>    [
            'mysql'    =>    [
                'type'        => 'mysql', // Database type
                'hostname'    => '127.0.0.1',// Server address
                'database'    => 'lavaman',// Database name
                'username'    => 'root',// Database username
                'password'    => 'root',// Database password
                'hostport'    => '',// Database connection port
                'params'      => [],
                'charset'     => 'utf8mb4',// Database encoding default utf8
                'prefix'      => '',// Table prefix
            ],
        ],
    ],
    'cookie' => [
        'expires' => 0,
        'path' => '/',
        'domain' => '',
        'secure' => true,
        'httponly' => true,
        'samesite' => 'Lax'  // None, Lax, Strict
    ]
];