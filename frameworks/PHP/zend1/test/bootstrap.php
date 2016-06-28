<?php

ini_set('error_reporting', -1);
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
ini_set('log_errors', 0);
ini_set('date.timezone', 'UTC');
ini_set('max_execution_time', 0);

// Define path to application directory
defined('APPLICATION_PATH')
    || define('APPLICATION_PATH', realpath(dirname(__FILE__) . '/../application'));

// Define application environment
defined('APPLICATION_ENV')
    || define('APPLICATION_ENV', (getenv('APPLICATION_ENV') ? getenv('APPLICATION_ENV') : 'testing'));

// Ensure library/ is on include_path
set_include_path(realpath(dirname(__FILE__) . '/../vendor/zendframework/zendframework1/library'));

require_once 'Zend/Loader/Autoloader.php';
Zend_Loader_Autoloader::getInstance();
