<?php
define('DS', DIRECTORY_SEPARATOR);
define('ROOT', __DIR__ . DS . 'app' . DS);
$config = include ROOT . 'config/config.php';
require ROOT . './../vendor/autoload.php';
\Ubiquity\controllers\Startup::run($config);
