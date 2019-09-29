<?php

$serverConfig = [
    'host' => env('SERVER_HOST', '0.0.0.0'),
    'port' => envInt('SERVER_PORT', 9501),
    'worker_num' => envInt('SERVER_WORKER_NUM', swoole_cpu_num() * 2),
    'daemonize' => envBool('SERVER_DAEMONIZE', false),
    'backlog' => envInt('SERVER_BACKLOG', 128),
    'max_request' => envInt('SERVER_MAX_REQUEST', 0),
    'dispatch_mode' => envInt('SERVER_DISPATCH_MODE', 2),
    'open_http2_protocol' => envBool('SERVER_OPEN_HTTP2', false),
    'task_worker_num' => envInt('SERVER_TASK_WORKER_NUM', 0),
    'task_enable_coroutine' => envBool('SERVER_TASK_ENABLE_COROUTINE', false),
    'open_tcp_nodelay' => envBool('SERVER_OPEN_TCP_NODELAY', true),
    'max_coroutine' => envInt('SERVER_MAX_COROUTINE', 1000000),
    'socket_buffer_size' => envInt('SERVER_SOCKET_BUFFER_SIZE', 2 * 1024 * 1024),
];

if (!empty($pidFile = env('SERVER_PID_FILE'))) {
    $serverConfig['pid_file'] = $pidFile;
}

return $serverConfig;
