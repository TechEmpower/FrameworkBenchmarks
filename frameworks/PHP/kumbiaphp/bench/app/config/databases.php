<?php
//Conexión a Mysql
return ['default' => [
            'dsn' => 'mysql:host=TFB-database;dbname=hello_world',
            'username' => 'benchmarkdbuser',
            'password' => 'benchmarkdbpass',
            'params' => [
                PDO::ATTR_PERSISTENT => true, //conexión persistente
            ]
        ],
    ];