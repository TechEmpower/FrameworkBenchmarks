<?php

use MongoDB\Client;
use Phalcon\Db\Adapter\Pdo\Mysql;
use Phalcon\DI\FactoryDefault;
use Phalcon\Exception as PhalconException;
use Phalcon\Http\Request;
use Phalcon\Incubator\MongoDB\Mvc\Collection\Manager as MongoDBCollectionManager;
use Phalcon\Loader;
use Phalcon\Mvc\Application;
use Phalcon\Mvc\Model\MetaData\Apc;
use Phalcon\Mvc\Model\MetaData\Memory;
use Phalcon\Mvc\View;
use Phalcon\Mvc\View\Engine\Volt;

define('APP_PATH', realpath('..'));
require APP_PATH . "/vendor/autoload.php";

try {
    // Load the config
    $config = include APP_PATH . "/app/config/config.php";

    // Register an autoloader
    $loader = new Loader();
    $loader->registerDirs([
        $config->application->controllersDir,
        $config->application->modelsDir,
        $config->application->collectionsDir,
    ])->register();

    // Create a DI
    $di = new FactoryDefault();

    // Setting up the router
    $di->setShared('router', require APP_PATH . '/app/config/routes.php');

    //MetaData
    $di->setShared('modelsMetadata', function () {
        if (function_exists('apc_store')) {
            return new Apc();
        }

        return new Memory([
            'metaDataDir' => APP_PATH . "/app/compiled-templates/"
        ]);
    });

    // Setting up the view component (seems to be required even when not used)
    $di->setShared('view', function () use ($config) {
        $view = new View();
        $view->setViewsDir($config->application->viewsDir);
        $view->registerEngines([
            ".volt" => function ($view) {
                $volt = new Volt($view);
                $volt->setOptions([
                    "path" => APP_PATH . "/app/compiled-templates/",
                    "extension" => ".compiled",
                    "separator" => '_',
                ]);

                return $volt;
            }
        ]);

        return $view;
    });

    // Setting up the database connection
    $di->setShared('db', function () use ($config) {
        $database = $config->database;

        return new Mysql([
            'host' => $database->host,
            'username' => $database->username,
            'password' => $database->password,
            'dbname' => $database->name,
            'options' => [
                PDO::MYSQL_ATTR_INIT_COMMAND => 'SET NAMES utf8',
                PDO::ATTR_PERSISTENT => true,
            ],
        ]);
    });

    // Setting up the mongodb connection
    $di->setShared('mongo', function () use ($config) {
        $mongodbConfig = $config->mongodb;
        $mongo = new Client($mongodbConfig->url);

        return $mongo->selectDatabase($mongodbConfig->db);
    });

    // Registering the mongoDB CollectionManager service
    $di->setShared('collectionsManager', function () {
        return new MongoDBCollectionManager();
    });

    // Handle the request
    $request = new Request();
    $application = new Application();
    $application->setDI($di);
    $application->handle($request->getURI())->send();
} catch (PhalconException $e) {
    echo "PhalconException: ", $e->getMessage();
}
