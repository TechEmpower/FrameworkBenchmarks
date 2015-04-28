<?php

/*
| -------------------------------------------------------------
| Check PHP Version
| -------------------------------------------------------------
| Check minimum version requirement of Cygnite
| and trigger exception is not satisfied
|
*/
if (version_compare(PHP_VERSION, '5.3', '<') === true) {
    @set_magic_quotes_runtime(0); // Kill magic quotes
    die('Require PHP v5.3.8 or More! \n');
}

/*
|--------------------------------------------------------------------------
| Check Extensions
|--------------------------------------------------------------------------
|
| Cygnite requires a few extensions to function. We will check if
| extensions loaded. If not we'll just exit from here.
|
*/
if ( ! extension_loaded('mcrypt')) {
    echo 'Cygnite requires Mcrypt PHP extension.'.PHP_EOL;
    exit(1);
}

require 'initialize'.EXT;

/*
|--------------------------------------------------------------------------
| Bootstrap process
|--------------------------------------------------------------------------
|
| Booting process is done lets start the application
*/
return $app->run();