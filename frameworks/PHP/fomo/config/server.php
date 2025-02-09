<?php

return [
    /*
     * mode
     * SWOOLE_BASE ,
     * SWOOLE_PROCESS
     */
    'mode' => SWOOLE_BASE ,
    'host' => '0.0.0.0',
    'port' => 9000 ,
    'sockType' => SWOOLE_SOCK_TCP ,
    'additional' => [
        'worker_num' => env('APP_WORKER_COUNT' , cpuCount() * 2) ,
        /*
         * log level
         * SWOOLE_LOG_DEBUG (default)
         * SWOOLE_LOG_TRACE
         * SWOOLE_LOG_INFO
         * SWOOLE_LOG_NOTICE
         * SWOOLE_LOG_WARNING
         * SWOOLE_LOG_ERROR
         */
        'log_level' => SWOOLE_LOG_DEBUG ,
        'log_file' => storagePath('logs/fomo.log') ,
    
        /*
        This key causes Fomo to receive a complete HTTP data packet and prevents it from receiving incomplete HTTP packets.
        */
        'open_http_protocol' => true,
    ],

    'ssl' => [
        'ssl_cert_file' => null ,
        'ssl_key_file' => null ,
    ] ,

    /*
     * The following services are created for better performance in the program, only one object is created from them and they can be used throughout the program
     */
    'services' => [
        Fomo\Services\Cache::class ,
        Fomo\Services\Database::class ,
        Fomo\Services\Language::class ,
        Fomo\Services\Response::class ,
        Fomo\Services\Validation::class ,
    ] ,

    /*
     * Files and folders that must be changed in real time
     */
    'watcher' => [
        'app',
        'config',
        'database',
        'language',
        'routes',
        'composer.lock',
        '.env',
    ] ,

    /*
     * Each of the following causes changes to the performance of the desired class. (so be careful in using them)
     */
    'advanceMode' => [
        /*
         * advanced mode in Fomo\Request\Request class
         *
         * By activating the advanced mode in this class, you can access the data you want in an advanced way
         * For example, consider that the user has sent you a array of the information of several customers.
         * If the advanced mode is not active, you can only access an array of all customer information
         *
         * For example, the:
         * $request->get('customers')
         *
         * But if the advanced mode is active, you can access any data you need from customers
         * For example, the:
         * $request->get('customers.*.name')
         */
        'request' => DISABLE
    ]
];
