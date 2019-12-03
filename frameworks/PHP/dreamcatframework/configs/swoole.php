<?php

# 启动worker进程前读取的配置
return [
    "host" => "0.0.0.0",
    "port" => 8080,
    "settings" => [
        "log_file" => null,
        "worker_num" => swoole_cpu_num(),
        "reactory_num" => null,
        "daemonize" => null,
        "log_level" => null,
        "pid_file" => null,
    ],
];

# end of file
