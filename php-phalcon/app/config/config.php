<?php

return new \Phalcon\Config(array(
    'database'     => array(
        'adapter'  => 'Mysql',
        'host'     => 'localhost',
        'username' => 'benchmarkdbuser',
        'password' => 'benchmarkdbpass',
        'name'     => 'hello_world',
    ),
    'application' => array(
        'controllersDir' => APP_PATH . '/app/controllers/',
        'modelsDir'      => APP_PATH . '/app/models/',
        'viewsDir'       => APP_PATH . '/app/views/',
        'baseUri'        => '/',
    )
));