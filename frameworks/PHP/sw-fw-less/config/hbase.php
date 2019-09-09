<?php

return [
    'pool_size' => envInt('HBASE_POOL_SIZE', 5),
    'switch' => envInt('HBASE_SWITCH', 0),
    'host' => env('HBASE_HOST', '127.0.0.1'),
    'port' => envInt('HBASE_PORT', 9090),
    'read_timeout' => envInt('HBASE_READ_TIMEOUT', 5000),
    'write_timeout' => envInt('HBASE_WRITE_TIMEOUT', 5000),
    'pool_change_event' => envInt('HBASE_POOL_CHANGE_EVENT', 0),
    'report_pool_change' => envInt('HBASE_REPORT_POOL_CHANGE', 0),
    'socket_driver' => env('HBASE_SOCKET_DRIVER', \SwFwLess\components\thrift\TCoroutineSocket::class),
];
