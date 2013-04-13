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
        'controllersDir' => __DIR__ . '/../../app/controllers/',
        'modelsDir'      => __DIR__ . '/../../app/models/',
        'viewsDir'       => __DIR__ . '/../../app/views/',
        'routes'         => __DIR__ . '/../../app/config/routes.php',
        'baseUri'        => '/',
    )
));