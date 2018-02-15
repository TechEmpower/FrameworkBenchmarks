<?php
//Conexión a Mysql
return ['default' => [
            'dsn' => 'mysql:host=localhost;dbname=hello_world',
            'username' => 'benchmarkdbuser',
            'password' => 'benchmarkdbpass',
            'params' => [
                PDO::ATTR_PERSISTENT => true, //conexión persistente
            ]
        ],
    ];