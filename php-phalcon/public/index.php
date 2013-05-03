<?php

define('APP_PATH', realpath('..'));

try {

    // Load the config
    $config = include APP_PATH . "/app/config/config.php";

    // Register an autoloader
    $loader = new \Phalcon\Loader();
    $loader->registerDirs(array(
        $config->application->controllersDir,
        $config->application->modelsDir
    ))->register();

    // Create a DI
    $di = new Phalcon\DI\FactoryDefault();

    // Setting up the router
    $di->set('router', require APP_PATH . '/app/config/routes.php');

    //MetaData
    $di->set('modelsMetadata', function(){
        if (function_exists('apc_store')) {
            return new Phalcon\Mvc\Model\MetaData\Apc();
        } else {
            return new Phalcon\Mvc\Model\MetaData\Files(array(
                'metaDataDir' => APP_PATH . "/app/compiled-templates/"
            ));
        }
    });

    // Setting up the view component (seems to be required even when not used)
    $di->set('view', function() use ($config) {

        $view = new \Phalcon\Mvc\View();

        $view->setViewsDir($config->application->viewsDir);

        $view->registerEngines(array(
            ".volt" => function($view, $di) {

                $volt = new \Phalcon\Mvc\View\Engine\Volt($view, $di);

                $volt->setOptions(array(
                    "compiledPath" => APP_PATH . "/app/compiled-templates/",
                    "compiledExtension" => ".compiled",
                    "compiledSeparator" => '_',
                ));

                return $volt;
            }
        ));

        return $view;
    });

    // Setting up the database connection
    $di->set('db', function() use ($config) {

        $database = $config->database;

        return new \Phalcon\Db\Adapter\Pdo\Mysql(array(
            'host' => $database->host,
            'username' => $database->username,
            'password' => $database->password,
            'dbname' => $database->name,
            'options'  => array(
                PDO::MYSQL_ATTR_INIT_COMMAND => 'SET NAMES utf8'
            )
        ));
    });

    // Handle the request
    $application = new \Phalcon\Mvc\Application();
    $application->setDI($di);
    echo $application->handle()->getContent();

} catch(\Phalcon\Exception $e) {
    echo "PhalconException: ", $e->getMessage();
}
