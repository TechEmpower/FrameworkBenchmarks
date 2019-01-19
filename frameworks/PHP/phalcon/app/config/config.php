<?php

return new \Phalcon\Config(array(
    'database'     => array(
        'adapter'  => 'Mysql',
        'host'     => 'tfb-database',
        'username' => 'benchmarkdbuser',
        'password' => 'benchmarkdbpass',
        'name'     => 'hello_world',
        'persistent' => true,
    ),
    'mongodb'     => array(
        'url'     => 'mongodb://tfb-database:27017',
        'db'      => 'hello_world'
    ),
    'application' => array(
        'controllersDir' => APP_PATH . '/app/controllers/',
        'modelsDir'      => APP_PATH . '/app/models/',
        'viewsDir'       => APP_PATH . '/app/views/',
        'baseUri'        => '/',
    )
));