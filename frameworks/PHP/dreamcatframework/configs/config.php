<?php

use Dreamcat\Components\Db\Mysql\MysqlInterface;
use DreamCat\FrameCore\Helper\ConfigHelper\BeansConfig;
use DreamCat\FrameCore\Helper\ConfigHelper\RouterConfig;
use DreamCat\FrameDbFactory\DefaultDbFactory;
use DreamCat\FrameDemo\Controller\DemoCtl;

return [
    # 路由配置
    "router" => [
        "prefix" => "/api",
        "uris" => [
            "/demo" => [
                "/count" => RouterConfig::get(DemoCtl::class),
                "/query/{name}" => RouterConfig::get(DemoCtl::class, "queryUser"),
            ],
        ],
    ],
    # 容器配置
    "beans" => [
        MysqlInterface::class => BeansConfig::factory(DefaultDbFactory::class),
    ],
    # 数据库配置，这个配置最好放在 cache/config.php 中，否则各环境会互相修改
    "db" => [
        # 数据库地址
        "host" => "127.0.0.1",
        # 数据库用户名
        "user" => "root",
        # 数据库密码
        "pass" => "123456",
        # 数据库名
        "db" => "mysql",
    ],
    # 日志配置，这个配置也大概率会各环境不一，建议放在 cache/config.php
    "log" => [
        "common" => [
            "fileName" => "common.log",
            "maxFile" => 10,
            "level" => Monolog\Logger::DEBUG,
        ],
        "error" => [
            "fileName" => "error.log",
            "maxFile" => 15,
            "level" => Monolog\Logger::ERROR,
        ],
    ],
];

# end of file
