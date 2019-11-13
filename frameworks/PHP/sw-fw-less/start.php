<?php

!defined('APP_BASE_PATH') && define('APP_BASE_PATH', __DIR__ . '/');

if (extension_loaded('jsonnet')) {
    !defined('CONFIG_FORMAT') && define('CONFIG_FORMAT', 'array,jsonnet');
}

require_once __DIR__ . '/vendor/autoload.php';

//This app supports hot reload and shutdown triggered by SIGTERM
(new \SwFwLess\bootstrap\App())->run();
