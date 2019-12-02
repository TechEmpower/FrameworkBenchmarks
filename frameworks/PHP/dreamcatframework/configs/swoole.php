<?php

# 启动worker进程前读取的配置
return [
    "port" => 8080,
    "settings" => [
        "log_file" => null,
        "worker_num" => swoole_cpu_num(),
        "reactory_num" => null,
    ],
];

# end of file
