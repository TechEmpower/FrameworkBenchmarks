<?php

use Phalcon\Db\Adapter\MongoDB\Client;
use Phalcon\Db\Adapter\Pdo\Mysql;
use Phalcon\Http\Request;
use Phalcon\Loader;
use Phalcon\Mvc\Application;
use Phalcon\Mvc\View\Engine\Volt;
use Phalcon\Mvc\View;
use Phalcon\Exception as PhalconException;

define('APP_PATH', realpath('..'));
require APP_PATH . "/vendor/autoload.php";

try {
    // Load the config
    $config = include APP_PATH . "/app/config/config.php";

    // Register an autoloader
    $loader = new Loader();
    $loader->registerDirs([
        $config->application->controllersDir,
        $config->application->modelsDir
    ])->register();

    // Create a DI
    $di = new Phalcon\DI\FactoryDefault();

    // Setting up the router
    $di->set('router', require APP_PATH . '/app/config/routes.php');

    //MetaData
    $di->set('modelsMetadata', function(){
        if (function_exists('apc_store')) {
            return new Phalcon\Mvc\Model\MetaData\Apc();
        }

        return new Phalcon\Mvc\Model\MetaData\Memory([
            'metaDataDir' => APP_PATH . "/app/compiled-templates/"
        ]);
    });

    // Setting up the view component (seems to be required even when not used)
    $di->set('view', function() use ($config) {
        $view = new View();
        $view->setViewsDir($config->application->viewsDir);
        $view->registerEngines([
            ".volt" => function($view) {
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
    $di->set('db', function() use ($config) {
        $database = $config->database;

        return new Mysql([
            'host' => $database->host,
            'username' => $database->username,
            'password' => $database->password,
            'dbname' => $database->name,
            'options'  => [
                PDO::MYSQL_ATTR_INIT_COMMAND => 'SET NAMES utf8',
                PDO::ATTR_PERSISTENT => true,
            ],
        ]);
    });

    // Setting up the mongodb connection
    $di->set('mongo', function() use ($config) {
        $mongodbConfig = $config->mongodb;

        $mongo = new Client($mongodbConfig->url);
        return $mongo->selectDatabase($mongodbConfig->db);
    });

    //Registering the collectionManager service
    $di->set('collectionManager', function() {
        // Setting a default EventsManager
        $modelsManager = new Phalcon\Mvc\Collection\Manager();
        $modelsManager->setEventsManager(new Phalcon\Events\Manager());

        return $modelsManager;
    }, true);

    // Handle the request
    $request = new Request();
    $application = new Application();
    $application->setDI($di);
    $application->handle($request->getURI())->send();
} catch(PhalconException $e) {
    echo "PhalconException: ", $e->getMessage();
}
