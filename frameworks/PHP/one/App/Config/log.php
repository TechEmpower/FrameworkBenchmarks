<?php

return [
    'path'    => _APP_PATH_ . '/RunCache/log', // 存放日志目录
    'id'      => uuid(),                       // 每个请求的日志里的唯一id
    'fn_save' => function ($code, $trace_id, $file, $line, $data) {
        echo $data . PHP_EOL;
    }

];