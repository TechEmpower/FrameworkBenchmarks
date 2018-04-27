<?php

return array(
    'db' => array(
        'driver'   => 'Pdo',
        'dsn'      => 'mysql:dbname=hello_world;host=tfb-database;port=3306',
        'username' => 'benchmarkdbuser',
        'password' => 'benchmarkdbpass',
        'driver_options' => array(
             PDO::ATTR_PERSISTENT => true
        ),
    ),
);
