<?php

# 在 swoole 体系中，用来修改 config.php 的配置
use Dreamcat\Components\Db\Mysql\MysqlInterface;
use DreamCat\FrameDbFactory\DefaultDbFactory;
use DreamCat\FrameSwoole\Factory\Impl\MysqlPool\MysqlSinglePoolEntryFactory;
use DreamCat\FrameSwoole\Helper\ConfigHelper\BeansConfig;

return [
    # 容器配置
    "beans" => [
        MysqlInterface::class => BeansConfig::factory(MysqlSinglePoolEntryFactory::class),
        MysqlSinglePoolEntryFactory::class => BeansConfig::byClass(MysqlSinglePoolEntryFactory::class, [], [], true, true),
        DefaultDbFactory::class => BeansConfig::byClass(DefaultDbFactory::class, [], [], false),
    ],
    # 池子的配置
    "pool" => ["mysql" => ["size" => 20]],
];

# end of file
