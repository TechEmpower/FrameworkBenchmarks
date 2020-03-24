<?php

// comment out the following two lines when deployed to production
//defined('YII_DEBUG') or define('YII_DEBUG', false);
//defined('YII_ENV') or define('YII_ENV', 'prod');
//error_reporting(E_ALL);
//ini_set('display_errors','on');

require(__DIR__ . '/../vendor/yiisoft/yii2/Yii.php');

$config = [
    'id' => 'basic',
    'basePath' => __DIR__,
    'components' => [
        'db' => [
            'class' => 'yii\db\Connection',
            'dsn' => 'mysql:host=tfb-database;dbname=hello_world',
            'username' => 'benchmarkdbuser',
            'password' => 'benchmarkdbpass',
            'charset' => 'utf8',
            'attributes' => [
                PDO::ATTR_PERSISTENT => true,
            ],
            'enableLogging' => false,
            'enableProfiling' => false,
            'enableSchemaCache' => true,
            'schemaCache' => 'cache',
            'schemaCacheDuration' => 3600,
        ],
        'cache' => [
            'class' => 'yii\caching\FileCache',
            'cachePath' => '/tmp/yii2-cache',
        ],
        'urlManager' => [
            'enablePrettyUrl' => true,
        ],
    ],
];

(new yii\web\Application($config))->run();
