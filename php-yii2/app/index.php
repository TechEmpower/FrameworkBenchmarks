<?php

// comment out the following two lines when deployed to production
//defined('YII_DEBUG') or define('YII_DEBUG', false);
//defined('YII_ENV') or define('YII_ENV', 'prod');
//error_reporting(E_ALL);
//ini_set('display_errors','on');

require(__DIR__ . '/vendor/yiisoft/yii2/Yii.php');

$config = [
    'id' => 'basic',
    'basePath' => __DIR__,
    'components' => [
        'db' => [
            'class' => 'yii\db\Connection',
            'dsn' => 'mysql:host=127.0.0.1;dbname=hello_world',
            'username' => 'benchmarkdbuser',
            'password' => 'benchmarkdbpass',
            'charset' => 'utf8',
        ],
        'urlManager' => [
            'enablePrettyUrl' => true,
        ],
    ],
];

(new yii\web\Application($config))->run();
