<?php
error_reporting(-1);

require_once __DIR__.'/vendor/autoload.php';

$app = new \Slim\App;

// Test 1: JSON serialization
$app->get('/json', function ($request, $response) {
    return $response
        ->withJson(array('message' => 'Hello, World!'))
        ->withHeader('Content-Type', 'application/json') // fixes utf-8 warning
        ;
});

$container = $app->getContainer();
$container['db'] = function ($c) {
    $db = $c['settings']['db'];
    $pdo = new PDO('mysql:host=localhost;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass');
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
    $pdo->setAttribute(PDO::ATTR_DEFAULT_FETCH_MODE, PDO::FETCH_ASSOC);
    return $pdo;
};

// Test 2: Single database query
$app->get('/db', function ($request, $response) {
    $sth = $this->db->prepare('SELECT * FROM World WHERE id = ?');
    $sth->execute(array(mt_rand(1, 10000)));
    $world = $sth->fetch();
    # Cast fields to int so they don't get wrapped with quotes
    $world['id'] = (int) $world['id'];
    $world['randomNumber'] = (int) $world['randomNumber'];

    return $response
        ->withJson($world)
        ->withHeader('Content-Type', 'application/json') // fixes utf-8 warning
        ;
});

// Test 3: Multiple database queries
$app->get('/dbs', function ($request, $response) {
    $queries = max(1, min($request->getParam('queries'), 500));

    $sth = $this->db->prepare('SELECT * FROM World WHERE id = ?');
    $worlds = array();
    for ($i = 0; $i < $queries; ++$i) {
        $sth->execute(array(mt_rand(1, 10000)));
        $world = $sth->fetch();
        # Cast fields to int so they don't get wrapped with quotes
        $world['id'] = (int) $world['id'];
        $world['randomNumber'] = (int) $world['randomNumber'];
        $worlds[] = $world;
    }

    return $response
        ->withJson($worlds)
        ->withHeader('Content-Type', 'application/json') // fixes utf-8 warning
        ;
});

$app->run();
