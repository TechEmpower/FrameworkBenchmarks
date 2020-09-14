<?php
return [
    'drive' => 'file', // [file | redis] 调用Cache:: 相关方法使用的缓存驱动

    'file' => [
        'path' => _APP_PATH_ . '/RunCache/cache', //文件缓存位置
        'prefix' => 'one_' //文件前缀
    ],
];

