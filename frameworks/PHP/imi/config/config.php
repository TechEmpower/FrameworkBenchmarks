<?php
$dbResourceConfig = [
    'host'        => 'tfb-database',
    'username'    => 'benchmarkdbuser',
    'password'    => 'benchmarkdbpass',
    'database'    => 'hello_world',
];
return [
    // 项目根命名空间
    'namespace'    =>    'ImiApp',

    // 配置文件
    'configs'    =>    [
        'beans'        =>    __DIR__ . '/beans.php',
    ],

    // 扫描目录
    'beanScan'    =>    [],

    // 组件命名空间
    'components'    =>  [],

    // 主服务器配置
    'mainServer'    =>    [
        'namespace' =>  'ImiApp\ApiServer',
        'type'      =>  Imi\Server\Type::HTTP,
        'host'      =>  '0.0.0.0',
        'port'      =>  8080,
        'mode'      =>  SWOOLE_BASE,
        'configs'   =>  [
            'worker_num'        => swoole_cpu_num(),
            'open_tcp_nodelay'  => true,
            'tcp_fastopen'      => true,
        ],
    ],

    'db'    => [
        'defaultPool'   => 'db', // 默认连接池
    ],
    'pools' => [
        // 连接池名称
        'db' => [
            'sync' => [
                'pool'    =>    [
                    'class'        =>    \Imi\Db\Pool\SyncDbPool::class,
                    'config'    =>    [
                        // 池子中最多资源数
                        'maxResources' => 512,
                        // 池子中最少资源数
                        'minResources' => 0,
                        'gcInterval'   => null,
                        'checkStateWhenGetResource' =>  false,
                    ],
                ],
                // resource也可以定义多个连接
                'resource'    =>    $dbResourceConfig,
            ],
            // 异步池子，worker进程使用
            'async' => [
                'pool'    =>    [
                    'class'        =>    \Imi\Db\Pool\CoroutineDbPool::class,
                    'config'    =>    [
                        // 池子中最多资源数
                        'maxResources' => 512,
                        // 池子中最少资源数
                        'minResources' => 16,
                        'gcInterval'   => null,
                        'checkStateWhenGetResource' =>  false,
                    ],
                ],
                // resource也可以定义多个连接
                'resource'    =>    $dbResourceConfig,
            ],
        ],
    ],
];
