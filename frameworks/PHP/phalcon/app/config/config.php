<?php

return new \Phalcon\Config(array(
    'database'     => array(
        'adapter'  => 'Mysql',
        'host'     => 'TFB-database',
        'username' => 'benchmarkdbuser',
        'password' => 'benchmarkdbpass',
        'name'     => 'hello_world',
    ),
    'mongodb'     => array(
        'url'     => 'mongodb://TFB-database:27017',
        'db'      => 'hello_world'
    ),
    'application' => array(
        'controllersDir' => APP_PATH . '/app/controllers/',
        'modelsDir'      => APP_PATH . '/app/models/',
        'viewsDir'       => APP_PATH . '/app/views/',
        'baseUri'        => '/',
    )
));