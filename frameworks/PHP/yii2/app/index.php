<?php

// uncomment the following block to debug this app
/*
defined('YII_DEBUG') or define('YII_DEBUG', true);
defined('YII_ENV') or define('YII_ENV', 'dev');
error_reporting(E_ALL);
ini_set('display_errors','on');
 */

define('YII_ENABLE_ERROR_HANDLER', false);

require __DIR__ . '/../vendor/yiisoft/yii2/Yii.php';

$config = [
    'id' => 'benchmark',
    'basePath' => __DIR__,
    'components' => [
        // Functions are faster than array declarations,
        // since they avoid the cost of Dependency Injection.
        'db' => function() {
            return new yii\db\Connection([
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
            ]);
        },
        'cache' => function() {
            return new yii\caching\FileCache([
                'cachePath' => '/tmp/yii2-cache',
                'gcProbability' => 0,
            ]);
        },
        'urlManager' => function() {
            return new yii\web\UrlManager([
                'enablePrettyUrl' => true,
            ]);
        },
        // These components are overloaded for a small gain in performance (no DI)
        'request' => function() { return new yii\web\Request(); },
        'response' => function() { return new yii\web\Response(); },
    ],
];

(new yii\web\Application($config))->run();
