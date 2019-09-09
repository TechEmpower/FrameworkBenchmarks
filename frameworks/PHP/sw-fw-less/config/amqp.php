<?php

return [
    'pool_size' => envInt('AMQP_POOL_SIZE', 5),
    'switch' => envInt('AMQP_SWITCH', 0),
    'prefix' => env('AMQP_PREFIX', 'sw-fw-less:'),
    'channel_id' => envInt('AMQP_CHANNEL_ID', 1),
    'host' => env('AMQP_HOST', '127.0.0.1'),
    'port' => envInt('AMQP_PORT', 5672),
    'user' => env('AMQP_USER', 'guest'),
    'passwd' => env('AMQP_PASSWD', 'guest'),
    'vhost' => env('AMQP_VHOST', '/'),
    'locale' => env('AMQP_LOCALE', 'en_US'),
    'read_timeout' => envInt('AMQP_READ_TIMEOUT', 3),
    'keepalive' => envBool('AMQP_KEEPALIVE', false),
    'write_timeout' => envInt('AMQP_WRITE_TIMEOUT', 3),
    'heartbeat' => envInt('AMQP_HEARTBEAT', 0),
    'pool_change_event' => envInt('AMQP_POOL_CHANGE_EVENT', 0),
    'report_pool_change' => envInt('AMQP_REPORT_POOL_CHANGE', 0),
    'socket_driver' => env('AMQP_SOCKET_DRIVER', \SwFwLess\components\amqp\CoroutineSocketIO::class),
];
