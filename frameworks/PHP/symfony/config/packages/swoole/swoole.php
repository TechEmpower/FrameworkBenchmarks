<?php

$container->loadFromExtension('swoole', [
    'http_server' => [
        'port' => '8080',
        'host' => '0.0.0.0',
        'running_mode' => 'reactor',
        'hmr' => false,
        'static' => [
            'strategy' => false,
        ],
        'api' => false,
        'services' => [
            'cloudfront_proto_header_handler' => false,
            'trust_all_proxies_handler' => false,
            'debug_handler' => false,
            'entity_manager_handler' => false,
        ],
        'settings' => [
            'reactor_count' => swoole_cpu_num() * 2,
            'worker_count' => swoole_cpu_num() * 2,
            'task_worker_count' => swoole_cpu_num() * 2,

            'log_file' => '/dev/null',
            'log_level' => 'error',

            'buffer_output_size' => 10 * 1024 * 1024,
        ],
    ],
]);
