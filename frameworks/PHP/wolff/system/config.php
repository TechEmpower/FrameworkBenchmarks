<?php

return [
    'db' => [
        'dsn'      => 'mysql:host=tfb-database;dbname=hello_world',
        'username' => 'benchmarkdbuser',
        'password' => 'benchmarkdbpass',
    ],

    'language'       => 'en',

    'development_on' => false,
    'template_on'    => true,
    'cache_on'       => false,
    'stdlib_on'      => true,
    'maintenance_on' => false,
];
