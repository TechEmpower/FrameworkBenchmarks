#!/usr/bin/env php
<?php
$path = realpath(__DIR__.'/');
require $path . '/vendor/autoload.php';        // @DUCKPHP_HEADFILE

$options = [
    'path' => $path,
    
    //'host'=>'127.0.0.1',    // default is 127.0.0.1 uncomment or --host to override
    //'port'=>'8080',         // default is 8080 uncomment or  --port to override
];

DuckPhp\HttpServer\HttpServer::RunQuickly($options);
