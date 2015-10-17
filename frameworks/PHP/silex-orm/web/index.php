<?php

use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\HttpFoundation\Request;

use Dflydev\Silex\Provider\DoctrineOrm\DoctrineOrmServiceProvider;
use Silex\Provider\DoctrineServiceProvider;

error_reporting(-1);

$loader = require_once __DIR__.'/../vendor/autoload.php';

\Doctrine\Common\Annotations\AnnotationRegistry::registerLoader(array($loader, 'loadClass'));

$app = new Silex\Application();

$dbh = new PDO('mysql:host=192.168.100.102;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
    PDO::ATTR_PERSISTENT => true
));

$app->register(new Silex\Provider\DoctrineServiceProvider(), array(
    'db.options' => array(
    'pdo' => $dbh
    ),
));

$app->register(new DoctrineOrmServiceProvider, array(
    'orm.proxies_dir' => __DIR__.'/../proxies', // Not sure how or if this needs handling...
    'orm.em.options' => array(
        'mappings' => array(
            array(
                'type' => 'annotation',
                'namespace' => 'Entity',
                'path' => __DIR__.'/../src/Entity',
                'use_simple_annotation_reader' => false,
            ),
        ),
        'metadata_cache' => 'redis'
    ),
));

$app->get('/json', function() {
    return new JsonResponse(array('message' => 'Hello, World!'));
});

$app->get('/db', function(Request $request) use ($app) {
    $queries = $request->query->getInt('queries', 1);
    // possibility for micro enhancement could be the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
    $worlds = array();
    $repo = $app['orm.em']->getRepository('Entity\World');

    for ($i = 0; $i < $queries; ++$i) {
        $worlds[] =  $repo->find(mt_rand(1, 10000));
    }

    if ($queries == 1) {
        $worlds = $worlds[0];
    }

    return new JsonResponse($worlds);
});


$app->run();




