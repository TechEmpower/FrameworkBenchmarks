<?php
/*
 |-------------------------------------------------------------------
 | Register Composer PSR Auto Loader
 |-------------------------------------------------------------------
 |
 | Composer is the convenient way to auto load all dependencies and
 | classes. We will simple require it here, so that we don't need
 | worry about importing classes manually.
 */
require __DIR__ . "/../vendor/autoload.php";

/*
| -------------------------------------------------------------------
| Check if script is running via cli and return false
| -------------------------------------------------------------------
*/
$filename = preg_replace('#(\?.*)$#', '', $_SERVER['REQUEST_URI']);

if (php_sapi_name() === 'cli-server' && is_file($filename)) {
    return false;
}

/*
|--------------------------------------------------------------------------
| Create The Application
|--------------------------------------------------------------------------
|
| To boot framework first thing we will create a new application instance
| which serves glue for all the components, and binding components
| with the IoC container
*/
$app = \Cygnite\Foundation\Application::instance();

$app->importHelpers();
/*
|--------------------------------------------------------------------------
| Attach Exception handler to Event Listener
|--------------------------------------------------------------------------
|
| We will attach exception handler to event listener, so that if anything
| goes wrong it will catch exceptions.
*/
$app['app.event'] = function () use($app) {

    $event = $app->singleton('event', '\Cygnite\Base\EventHandler\Event');
    $event->attach("exception", '\\Cygnite\\Exception\\Handler@handleException');

    return $event;
};

$config = \Cygnite\Helpers\Config::load();

/**
 | ---------------------------------------------------
 | Application booting process
 | --------------------------------------------------
 *
 * Set configuration and services and boot-up application
 */
return $app->setConfiguration($config)->boot();