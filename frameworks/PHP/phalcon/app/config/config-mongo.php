<?php

use Phalcon\Config;

return new Config([
    'database'     => [
        'adapter'  => 'Mysql',
        'host'     => 'tfb-database',
        'username' => 'benchmarkdbuser',
        'password' => 'benchmarkdbpass',
        'name'     => 'hello_world',
        'persistent' => true,
    ],
    'mongodb'     => [
        'url'     => 'mongodb://tfb-database:27017',
        'db'      => 'hello_world'
    ],
    'application' => [
        'controllersDir' => APP_PATH . '/app/controllers/',
        'modelsDir'      => APP_PATH . '/app/models/',
        'collectionsDir' => APP_PATH . '/app/collections/',
        'viewsDir'       => APP_PATH . '/app/views/',
        'baseUri'        => '/',
    ]
]);
