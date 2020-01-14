<?php

return [
    \SwFwLess\components\redis\RedisPool::EVENT_REDIS_POOL_CHANGE => [
        function (\Cake\Event\Event $event) {
            $count = $event->getData('count');

            if (\SwFwLess\components\Config::get('redis.report_pool_change')) {
                \SwFwLess\components\swoole\counter\Counter::incr('monitor:pool:redis', $count);
            }
        },
    ],
    \SwFwLess\components\mysql\MysqlPool::EVENT_MYSQL_POOL_CHANGE => [
        function (\Cake\Event\Event $event) {
            $count = $event->getData('count');

            if (\SwFwLess\components\Config::get('mysql.report_pool_change')) {
                \SwFwLess\components\swoole\counter\Counter::incr('monitor:pool:mysql', $count);
            }
        },
    ],
    \SwFwLess\components\amqp\ConnectionPool::EVENT_AMQP_POOL_CHANGE => [
        function (\Cake\Event\Event $event) {
            $count = $event->getData('count');

            if (\SwFwLess\components\Config::get('amqp.report_pool_change')) {
                \SwFwLess\components\swoole\counter\Counter::incr('monitor:pool:amqp', $count);
            }
        },
    ],
    \SwFwLess\components\hbase\HbasePool::EVENT_HBASE_POOL_CHANGE => [
        function (\Cake\Event\Event $event) {
            $count = $event->getData('count');

            if (\SwFwLess\components\Config::get('hbase.report_pool_change')) {
                \SwFwLess\components\swoole\counter\Counter::incr('monitor:pool:hbase', $count);
            }
        },
    ],
];
