<?php
/**
 * Step 1: Require the Slim Framework
 *
 * If you are not using Composer, you need to require the
 * Slim Framework and register its PSR-0 autoloader.
 *
 * If you are using Composer, you can skip this step.
 */
require 'Slim/Slim.php';
require 'Slim/RedBean/rb.php';

\Slim\Slim::registerAutoloader();

R::setup('mysql:host=localhost;dbname=hello_world','benchmarkdbuser','benchmarkdbpass');
R::freeze(true);

/**
 * Step 2: Instantiate a Slim application
 *
 * This example instantiates a Slim application using
 * its default settings. However, you will usually configure
 * your Slim application now by passing an associative array
 * of setting names and values into the application constructor.
 */
$app = new \Slim\Slim();

/**
 * Step 3: Define the Slim application routes
 *
 * Here we define several Slim application routes that respond
 * to appropriate HTTP request methods. In this example, the second
 * argument for `Slim::get`, `Slim::post`, `Slim::put`, and `Slim::delete`
 * is an anonymous function.
 */

$app->get('/json', function () use($app) {
    $app->contentType('application/json');
    echo json_encode(array('message' => 'Hello, World!'));
});

$app->get('/db', function () use($app) {
    $queries = ($app->request()->get('queries') !== null)
        ? $app->request()->get('queries')
        : 1;
    $worlds = array();

    for ($i = 0; $i < $queries; ++$i) {
        $worlds[] = R::load('World', mt_rand(1, 10000))->export();
    }

    $app->contentType('application/json');
    echo json_encode($worlds);
});

/**
 * Step 4: Run the Slim application
 *
 * This method should be called last. This executes the Slim application
 * and returns the HTTP response to the HTTP client.
 */
$app->run();
