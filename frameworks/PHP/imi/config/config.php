<?php
$dbResourceConfig = [
    'host'        => 'tfb-database',
    'username'    => 'benchmarkdbuser',
    'password'    => 'benchmarkdbpass',
    'database'    => 'hello_world',
    'dbClass'     => \Imi\Db\Drivers\Swoole\Driver::class,
];
return [
    // 项目根命名空间
    'namespace'    =>    'ImiApp',

    // 配置文件
    'configs'    =>    [
        'beans'        =>    __DIR__ . '/beans.php',
    ],

    // 扫描目录
    'beanScan'    =>    [
        'ImiApp\Listener',
    ],

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
            'http_parse_post'   => false,
            'http_parse_cookie' => false,
            'http_parse_files'  => false,
            'http_compression'  => false,
        ],
    ],

    'db'    => [
        'defaultPool'   => 'db', // 默认连接池
    ],
    'redis'    =>    [
        'defaultPool'               =>    'redis', // 默认连接池
        'quickFromRequestContext'   =>    true, // 从当前上下文中获取公用连接
    ],
    'pools' => [
        // 连接池名称
        'db' => [
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
                        'requestResourceCheckInterval' => 30,
                    ],
                ],
                // resource也可以定义多个连接
                'resource'    =>    $dbResourceConfig,
            ],
        ],
        'redis' =>  [
            'pool' => [
                // 协程池类名
                'asyncClass'    =>    \Imi\Redis\CoroutineRedisPool::class,
                'config' => [
                    // 池子中最多资源数
                    'maxResources' => 512,
                    // 池子中最少资源数
                    'minResources' => 0,
                    'gcInterval'   => null,
                    'checkStateWhenGetResource' =>  false,
                    'requestResourceCheckInterval' => 30,
                ],
            ],
            // 数组资源配置
            'resource' => [
                'host'      =>  '127.0.0.1',
                'port'      =>  6379,
                // 是否自动序列化变量
                'serialize' =>  true,
                // 密码
                'password'  =>  null,
                // 第几个库
                'db'        =>  0,
            ],
        ],
    ],
];
