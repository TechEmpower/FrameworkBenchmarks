<?php

declare(strict_types=1);
/**
 * This file is part of Simps.
 *
 * @link     https://simps.io
 * @document https://doc.simps.io
 * @license  https://github.com/simple-swoole/simps/blob/master/LICENSE
 */

return [
    'mode' => SWOOLE_BASE,
    'http' => [
        'ip' => '0.0.0.0',
        'port' => 8080,
        'sock_type' => SWOOLE_SOCK_TCP,
        'callbacks' => [
        ],
        'settings' => [
            'worker_num' => swoole_cpu_num(),
            'only_simple_http' => true,
        ],
    ],
];