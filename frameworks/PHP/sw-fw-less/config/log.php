<?php

return [
    'path' => env('LOG_PATH', __DIR__ . '/../runtime/logs/app-{date}.log'),
    'level' => envInt('LOG_LEVEL', \Monolog\Logger::DEBUG),
    'pool_size' => envInt('LOG_POOL_SIZE', 100),
    'buffer_max_size' => envInt('LOG_BUFFER_MAX_SIZE', 10),
    'name' => env('LOG_NAME', 'sw-fw-less'),
    'reserve_days' => envInt('LOG_RESERVE_DAYS', 3),
    'switch' => envInt('LOG_SWITCH', 0),
];
