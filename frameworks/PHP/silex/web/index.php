<?php

use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\HttpFoundation\Request;

error_reporting(-1);

require_once __DIR__.'/../vendor/autoload.php';

$app = new Silex\Application();

$dbh = new PDO('mysql:host=localhost;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
    PDO::ATTR_PERSISTENT => true
));

$app->register(new Silex\Provider\DoctrineServiceProvider(), array(
    'db.options' => array(
    'pdo' => $dbh
    ),
));

$app->get('/json', function() {
    return new JsonResponse(array("message" => "Hello, World!"));
});

$app->get('/db', function(Request $request) use ($app) {
    $world = $app['db']->fetchAssoc('SELECT * FROM World WHERE id = ?', array(mt_rand(1, 10000)));
    $world['id'] = (int) $world['id'];
    $world['randomNumber'] = (int) $world['randomNumber'];

    return new JsonResponse($world);
});

$app->get('/queries', function(Request $request) use ($app) {
    $queries = $request->query->getInt('queries', 1);
    if ($queries < 1) {
        $queries = 1;
    }
    elseif ($queries > 500) {
        $queries = 500;
    }
    // possibility for micro enhancement could be the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
    $worlds = array();

    for($i = 0; $i < $queries; ++$i) {
        $world = $app['db']->fetchAssoc('SELECT * FROM World WHERE id = ?', array(mt_rand(1, 10000)));
        $world['id'] = (int) $world['id'];
        $world['randomNumber'] = (int) $world['randomNumber'];
        $worlds[] = $world;
    }

    return new JsonResponse($worlds);
});

$app->run();



