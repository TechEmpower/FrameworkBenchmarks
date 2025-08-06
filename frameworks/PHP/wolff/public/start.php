<?php

/**
 * Config
 */
global $config;
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

function run(): string
{
    global $config;
    ob_start();

    $_SERVER['DOCUMENT_ROOT'] = '/wolff/bench';
    new Wolff\Kernel($config)->start();
    header(HeaderDate::$date); // To pass the bench, nginx auto add it

    return ob_get_clean();
}
