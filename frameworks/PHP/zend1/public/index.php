<?php

// Define path to application directory
defined('APPLICATION_PATH')
    || define('APPLICATION_PATH', realpath(dirname(__FILE__) . '/../application'));

// Define application environment
defined('APPLICATION_ENV')
    || define('APPLICATION_ENV', (getenv('APPLICATION_ENV') ? getenv('APPLICATION_ENV') : 'production'));

set_include_path(realpath(dirname(__FILE__) . '/../vendor/zendframework/zendframework1/library'));

require_once 'Zend/Loader/Autoloader/Resource.php';
$resources = new Zend_Loader_Autoloader_Resource(array(
	'namespace' => '',
	'basePath' => APPLICATION_PATH
));
$resources->addResourceType('model', 'models', 'Model');

require_once 'Zend/Application.php';

// Create application, bootstrap, and run
$application = new Zend_Application(
    APPLICATION_ENV,
    APPLICATION_PATH . '/configs/application.ini'
);
$application->bootstrap()
            ->run();
