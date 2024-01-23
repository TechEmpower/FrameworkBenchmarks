<?php
error_reporting(-1);

//require_once __DIR__.'/vendor/autoload.php';

use DI\Container;
use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Factory\AppFactory;
use Slim\Views\PhpRenderer;
use Db\Raw;

global $app; // workerman

Raw::init();

$container = new Container();

$container->set('view', new PhpRenderer('templates'));

AppFactory::setContainer($container);
$app = AppFactory::create();


// Test 1: Plaintext
$app->get('/plaintext', fn(Request $request, Response $response) =>
    $response
        ->write('Hello, World!')
        ->withHeader('Content-Type', 'text/plain')
);

// Test 2: JSON serialization
$app->get('/json', fn(Request $request, Response $response) =>
    $response
        ->withJson(['message' => 'Hello, World!'])
);

// Test 3: Single database query
$app->get('/db', function (Request $request, Response $response) {
    
    Raw::$random->execute([mt_rand(1, 10000)]);

    return $response
        ->withJson(Raw::$random->fetch());
});

// Test 4: Multiple database queries
$app->get('/dbs', function (Request $request, Response $response) {
    $queries = $request->getParam('queries');
    if (is_numeric($queries)) {
        $queries = max(1, min($queries, 500));
    } else {
        $queries = 1;
    }

    $sth = Raw::$random;
    $worlds = [];
    for ($i = 0; $i < $queries; ++$i) {
        $sth->execute([mt_rand(1, 10000)]);
        $worlds[] = $sth->fetch();
    }

    return $response
        ->withJson($worlds);
});

// Test 5: Updates
$app->get('/updates', function (Request $request, Response $response) {
    $queries = $request->getParam('queries');
    if (is_numeric($queries)) {
        $queries = max(1, min($queries, 500));
    } else {
        $queries = 1;
    }

    $sth = Raw::$random;
    //$updateSth = Raw::update();

    $worlds = [];
    for ($i = 0; $i < $queries; ++$i) {
        $sth->execute([mt_rand(1, 10000)]);
        $world = $sth->fetch();
        $world['randomNumber'] = mt_rand(1, 10000);

        $worlds[] = $world;
    }

    Raw::update($worlds);

    return $response
        ->withJson($worlds);
});

// Test 6: Fortunes
$app->get('/fortunes', function (Request $request, Response $response) {
    Raw::$fortune->execute();
    $fortunes = Raw::$fortune->fetchAll(PDO::FETCH_KEY_PAIR);

    $fortunes[0] = 'Additional fortune added at request time.';
    asort($fortunes);

    return $this->get('view')->render($response, 'fortunes.php', ['fortunes' => $fortunes]);
});

//$app->run(); // comented with workerman

// used by Workerman
function run(): string
{
    global $app;
    ob_start();

    $app->run();
    header(HeaderDate::$date); // To pass the bench, nginx auto add it

    return ob_get_clean();
}
