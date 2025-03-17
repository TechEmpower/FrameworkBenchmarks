<?php
return [

    // Route configuration
    'route_path' =>  __DIR__ .'/app/route.php',

    // Application configuration file location can be modified freely

    // Key represents the name of the sub-application; sub-applications not listed cannot be accessed
    'config' => [
        // Default configuration
        '' => require 'app/config.php',
        // If a sub-application does not mention a configuration, the content of the default configuration file will be used
        // 'admin'=> (require 'app/admin/config.php') + (require 'app/config.php'),

        // Or only use the default configuration
        // 'phone'=> require 'app/config.php',

        // Or do not use the default configuration, directly use your custom sub-application configuration, you can change the name freely
        // 'phone'=> require 'app/config_phone.php',

        // Or this way, each configuration item is introduced separately
        // 'admin'=> [
        //     'app_name' => 'admin',
        //     'request_middleware' => require 'app/admin/config_request_middleware.php',
        //     'middleware' => require 'app/admin/config_middleware.php',
        //     'database' => require 'app/admin/config_database.php',
        //     'cookie' => require 'app/admin/config_cookie.php',
        //     'database' => require 'app/admin/config_database.php',
        // ],
    ],

    // Create route manager
    'Route' => \DI\create(\Cyber\Route::class),
    'Middleware' => \DI\create(\Cyber\Middleware::class),

    // Create request object for handling HTTP requests
    'Request' => \DI\create(\Cyber\Request::class),
    // Create response object for generating HTTP responses
    'Response' => \DI\create(\Cyber\Response::class),
];