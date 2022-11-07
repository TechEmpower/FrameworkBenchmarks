<?php
error_reporting(-1);

require_once __DIR__.'/vendor/autoload.php';

use DI\Container;
use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Factory\AppFactory;
use Slim\Views\PhpRenderer;

//global $app; // workerman

$container = new Container();

$container->set('db', new PDO(
                'mysql:host=tfb-database;dbname=hello_world;charset=utf8', 
            'benchmarkdbuser', 
            'benchmarkdbpass',
            [
                    PDO::ATTR_PERSISTENT          => true,
                    PDO::ATTR_DEFAULT_FETCH_MODE  => PDO::FETCH_ASSOC,
                    PDO::ATTR_ERRMODE             => PDO::ERRMODE_EXCEPTION,
                    //PDO::ATTR_EMULATE_PREPARES    => false, // workerman
            ]));

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
    $sth = $this->get('db')->prepare('SELECT * FROM World WHERE id = ?');
    $sth->execute([mt_rand(1, 10000)]);
    $world = $sth->fetch();
    # Cast fields to int so they don't get wrapped with quotes
    $world['id'] = (int) $world['id'];
    $world['randomNumber'] = (int) $world['randomNumber'];

    return $response
        ->withJson($world);
});

// Test 4: Multiple database queries
$app->get('/dbs', function (Request $request, Response $response) {
    $queries = $request->getParam('queries');
    if (is_numeric($queries)) {
        $queries = max(1, min($queries, 500));
    } else {
        $queries = 1;
    }

    $sth = $this->get('db')->prepare('SELECT * FROM World WHERE id = ?');
    $worlds = [];
    for ($i = 0; $i < $queries; ++$i) {
        $sth->execute([mt_rand(1, 10000)]);
        $world = $sth->fetch();
        # Cast fields to int so they don't get wrapped with quotes
        $world['id'] = (int) $world['id'];
        $world['randomNumber'] = (int) $world['randomNumber'];
        $worlds[] = $world;
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

    $sth = $this->get('db')->prepare('SELECT * FROM World WHERE id = ?');
    $updateSth = $this->get('db')->prepare('UPDATE World SET randomNumber = ? WHERE id = ?');

    $worlds = [];
    for ($i = 0; $i < $queries; ++$i) {
        $id = mt_rand(1, 10000);
        $random_number = mt_rand(1, 10000);
        $sth->execute([$id]);
        $world = $sth->fetch();
        # Cast fields to int so they don't get wrapped with quotes
        $world['id'] = (int) $world['id'];
        $world['randomNumber'] = $random_number;

        $updateSth->execute([$world['randomNumber'], $world['id']]);

        $worlds[] = $world;
    }

    return $response
        ->withJson($worlds);
});

// Test 6: Fortunes
$app->get('/fortunes', function (Request $request, Response $response) {
    $fortunes = $this->get('db')->query('SELECT * FROM Fortune')->fetchAll(PDO::FETCH_KEY_PAIR);

    $fortunes[0] = 'Additional fortune added at request time.';
    asort($fortunes);

    return $this->get('view')->render($response, 'fortunes.php', ['fortunes' => $fortunes]);
});

$app->run(); // comented with workerman

// used by Workerman
function run(): string
{
    global $app;
    ob_start();

    $app->run();
    header(HeaderDate::$date); // To pass the bench, nginx auto add it

    return ob_get_clean();
}
