<?php
/**
 * This file is part of webman.
 *
 * Licensed under The MIT License
 * For full copyright and license information, please see the MIT-LICENSE.txt
 * Redistributions of files must retain the above copyright notice.
 *
 * @author    walkor<walkor@workerman.net>
 * @copyright walkor<walkor@workerman.net>
 * @link      http://www.workerman.net/
 * @license   http://www.opensource.org/licenses/mit-license.php MIT License
 */

use support\view\Raw;
use support\view\Twig;
use support\view\Blade;
use support\view\ThinkPHP;

return [
    // 文件更新检测
    /*'monitor' => [
        'handler'     => process\FileMonitor::class,
        'constructor' => [
            // 监控这些目录
            'monitor_dir' => [
                app_path(),
                config_path(),
                base_path() . '/process',
                base_path() . '/support',
                base_path() . '/resource'
            ],
            // 监控这些后缀的文件
            'monitor_extenstions' => [
                'php', 'html', 'htm'
            ]
        ]
    ],*/
    // 其它进程
    /*'websocket'  => [
        'handler'  => process\Websocket::class,
        'listen' => 'websocket://0.0.0.0:8888',
        'count'  => 10,
    ],*/
];
