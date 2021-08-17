<?php

namespace App\Container;

use Mix\Database\Database;

class DB
{

    /**
     * @var Database
     */
    static private $instance;

    /**
     * @return Database
     */
    public static function instance(): Database
    {
        if (!isset(self::$instance)) {
            $dsn = 'mysql:host=tfb-database;dbname=hello_world';
            $username = 'benchmarkdbuser';
            $password = 'benchmarkdbpass';
            $db = new Database($dsn, $username, $password, [
                \PDO::ATTR_EMULATE_PREPARES => false,
                \PDO::ATTR_DEFAULT_FETCH_MODE => \PDO::FETCH_OBJ,
            ]);
            self::$instance = $db;
        }
        return self::$instance;
    }

    public static function enableCoroutine()
    {
        $maxOpen = intdiv(256, swoole_cpu_num());   // 最大开启连接数
        $maxIdle = 20;  // 最大闲置连接数
        $maxLifetime = 0;   // 连接的最长生命周期
        $waitTimeout = 0.0; // 从池获取连接等待的时间, 0为一直等待
        self::instance()->startPool($maxOpen, $maxIdle, $maxLifetime, $waitTimeout);
        \Swoole\Runtime::enableCoroutine(); // 必须放到最后，防止触发协程调度导致异常
    }

}
