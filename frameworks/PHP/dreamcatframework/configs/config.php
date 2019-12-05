<?php

use DreamCat\Benchmark\Controller\FortunesCtl;
use DreamCat\Benchmark\Controller\WorldTestCtl;
use DreamCat\Benchmark\Controller\PlainTextCtl;
use Dreamcat\Components\Db\Mysql\MysqlInterface;
use DreamCat\FrameCore\Helper\ConfigHelper\BeansConfig;
use DreamCat\FrameCore\Helper\ConfigHelper\RouterConfig;
use DreamCat\FrameDbFactory\DefaultDbFactory;
use DreamCat\Benchmark\Controller\JsonCtl;

return [
    # 路由配置
    "router" => [
        "prefix" => "",
        "uris" => [
            "/json" => RouterConfig::get(JsonCtl::class),
            "/plaintext" => RouterConfig::get(PlainTextCtl::class),
            "/db" => RouterConfig::get(WorldTestCtl::class),
            "/queries" => RouterConfig::get(WorldTestCtl::class, "queries"),
            "/fortunes" => RouterConfig::get(FortunesCtl::class),
            "/updates" => RouterConfig::get(WorldTestCtl::class, "updates"),
        ],
    ],
    # 容器配置
    "beans" => [
        MysqlInterface::class => BeansConfig::factory(DefaultDbFactory::class),
    ],
    # 数据库配置，这个配置最好放在 cache/config.php 中，否则各环境会互相修改
    "db" => [
        # 数据库地址
        "host" => gethostbyname("tfb-database"),
        # 数据库用户名
        "user" => "benchmarkdbuser",
        # 数据库密码
        "pass" => "benchmarkdbpass",
        # 数据库名
        "db" => "hello_world",
    ],
    # 日志配置，这个配置也大概率会各环境不一，建议放在 cache/config.php
    "log" => [
        "logs" => [],
    ],
];

# end of file
