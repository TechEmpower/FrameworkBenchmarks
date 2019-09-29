<?php

return [
    //Common Providers
    \SwFwLess\components\swoole\counter\CounterProvider::class,
//    \SwFwLess\components\redis\RedisProvider::class,

    //App Providers
    \SwFwLess\components\swoole\SwooleProvider::class,
//    \SwFwLess\components\chaos\ChaosProvider::class,

    //Worker Providers
    \SwFwLess\components\datetime\DatetimeProvider::class,
    \SwFwLess\components\event\EventProvider::class,
    \SwFwLess\components\log\LogProvider::class,
//    \SwFwLess\components\ratelimit\RatelimitProvider::class,
//    \SwFwLess\components\cache\CacheProvider::class,
    \SwFwLess\components\mysql\MysqlProvider::class,
//    \SwFwLess\components\es\EsProvider::class,
//    \SwFwLess\components\storage\StorageProvider::class,
//    \SwFwLess\components\amqp\AmqpProvider::class,
//    \SwFwLess\components\hbase\HbaseProvider::class,
    \SwFwLess\components\di\ContainerProvider::class,
//    \SwFwLess\components\auth\jwt\JwtProvider::class,

    //Request Providers

    //Shutdown Providers
    \SwFwLess\components\swoole\coresource\CoroutineResProvider::class,
];
