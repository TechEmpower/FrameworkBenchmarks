<?php

require '../vendor/autoload.php';

/**
 * Config
 */

$config = require '../system/config.php';

if ($config['stdlib_on']) {
    include_once '../vendor/usbac/wolff-framework/src/stdlib.php';
}

error_reporting($config['development_on'] ? E_ALL : 0);
ini_set('display_errors', strval($config['development_on']));

/**
 * Wolff
 */

require '../system/web.php';

$wolff = new Wolff\Kernel($config);
$wolff->start();
