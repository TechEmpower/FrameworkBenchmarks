<?php

use Imi\App;

$mode = App::isInited() ? App::getApp()->getType() : '';
$isMysql = ('mysql' === strtolower(getenv('TFB_TEST_DATABASE') ?: 'mysql'));
$host = 'tfb-database';
$username = 'benchmarkdbuser';
$password = 'benchmarkdbpass';

return [
    // 项目根命名空间
    'namespace'    =>    'ImiApp',

    // 配置文件
    'configs'    =>    [
        'beans'        =>    __DIR__ . '/beans.php',
    ],

    // 组件命名空间
    'components'    =>  [],

    // 主服务器配置
    'mainServer'    => 'swoole' === $mode ? [
        'namespace' =>  'ImiApp\ApiServer',
        'type'      =>  Imi\Swoole\Server\Type::HTTP,
        'host'      =>  '0.0.0.0',
        'port'      =>  8080,
        'mode'      =>  SWOOLE_BASE,
        'configs'   =>  [
            'worker_num'        => swoole_cpu_num(),
            'open_tcp_nodelay'  => true,
            'tcp_fastopen'      => true,
            'http_parse_post'   => false,
            'http_parse_cookie' => false,
            'http_parse_files'  => false,
            'http_compression'  => false,
        ],
    ] : [],

    // Workerman 服务器配置
    'workermanServer' => 'workerman' === $mode ? [
        // 服务器名，http 也可以改成 abc 等等，完全自定义
        'http' => [
            // 指定服务器命名空间
            'namespace' => 'ImiApp\ApiServer',
            // 服务器类型
            'type'      => Imi\Workerman\Server\Type::HTTP, // HTTP、WEBSOCKET、TCP、UDP
            'host'      => '0.0.0.0',
            'port'      => 8080,
            // socket的上下文选项，参考：http://doc3.workerman.net/315128
            'context'   => [],
            'configs'   => [
                // 支持设置 Workerman 参数
                'count' => (int) shell_exec('nproc') * 4,
            ],
        ],
    ] : [],

    'db'    => [
        'defaultPool'   => $isMysql ? 'mysql' : 'pgsql', // 默认连接池
        'connections'   => [
            'mysql' => [
                'host'                        => $host,
                'username'                    => $username,
                'password'                    => $password,
                'database'                    => 'hello_world',
                'dbClass'                     => \Imi\Db\Mysql\Drivers\Mysqli\Driver::class,
                'checkStateWhenGetResource'   => false,
            ],
            'pgsql' => [
                'host'                        => $host,
                'username'                    => $username,
                'password'                    => $password,
                'database'                    => 'hello_world',
                'dbClass'                     => \Imi\Pgsql\Db\Drivers\PdoPgsql\Driver::class,
                'checkStateWhenGetResource'   => false,
            ],
        ],
    ],

    'pools' => 'swoole' === $mode ? [
        // 连接池名称
        'mysql' => [
            'pool'    =>    [
                'class'        =>    \Imi\Swoole\Db\Pool\CoroutineDbPool::class,
                'config'    =>    [
                    // 池子中最多资源数
                    'maxResources' => intval(1024 / swoole_cpu_num()),
                    // 池子中最少资源数
                    'minResources' => $isMysql ? 16 : 0,
                    'gcInterval'   => 0,
                    'checkStateWhenGetResource' =>  false,
                ],
            ],
            // resource也可以定义多个连接
            'resource'    =>    [
                'host'        => $host,
                'username'    => $username,
                'password'    => $password,
                'database'    => 'hello_world',
                'dbClass'     => \Imi\Swoole\Db\Driver\Swoole\Driver::class,
            ],
        ],
        'pgsql' => [
            'pool'    =>    [
                'class'        =>    \Imi\Swoole\Db\Pool\CoroutineDbPool::class,
                'config'    =>    [
                    // 池子中最多资源数
                    'maxResources' => intval(1024 / swoole_cpu_num()),
                    // 池子中最少资源数
                    'minResources' => $isMysql ? 0 : 16,
                    'checkStateWhenGetResource' =>  false,
                ],
            ],
            // resource也可以定义多个连接
            'resource'    =>    [
                'host'        => $host,
                'username'    => $username,
                'password'    => $password,
                'database'    => 'hello_world',
                'dbClass'     => \Imi\Pgsql\Db\Drivers\Swoole\Driver::class,
            ],
        ],
    ] : [],
];
