<?php

use Phalcon\Db\Adapter\Pdo\Mysql;
use Phalcon\Db\Enum;
use Phalcon\Mvc\Micro;
use Phalcon\Mvc\View;
use Phalcon\Mvc\View\Engine\Volt;
use Phalcon\Exception as PhalconException;

try {
    $app = new Micro();

    // Setting up the database connection
    $app['db'] = function () {
        return new Mysql([
            'host' => 'tfb-database',
            'dbname' => 'hello_world',
            'username' => 'benchmarkdbuser',
            'password' => 'benchmarkdbpass',
            'options' => [
                PDO::MYSQL_ATTR_INIT_COMMAND => "SET NAMES 'UTF8'",
                PDO::ATTR_PERSISTENT => true,
            ],
        ]);
    };

    // Setting up the view component (seems to be required even when not used)
    $app['view'] = function () {
        $view = new View();
        $view->setViewsDir(__DIR__ . '/../app/views/');
        $view->registerEngines([
            ".volt" => function ($view) {
                $volt = new Volt($view);
                $volt->setOptions([
                    "path" => __DIR__ . "/../app/compiled-templates/",
                    "extension" => ".c",
                    "separator" => '_',
                ]);

                return $volt;
            }
        ]);

        return $view;
    };

    /**
     * Routes
     */
    $app->map('/plaintext', function () {
        header("Content-Type: text/plain; charset=UTF-8");
        echo "Hello, World!";
    });

    $app->map('/json', function () {
        header("Content-Type: application/json");
        echo json_encode(['message' => 'Hello, World!']);
    });

    $app->map('/db', function () use ($app) {
        $db = $app['db'];
        $world = $db->fetchOne('SELECT * FROM world WHERE id = ' . mt_rand(1, 10000), Enum::FETCH_ASSOC);

        header("Content-Type: application/json");
        echo json_encode($world);
    });

    $app->map('/queries', function () use ($app) {
        $db = $app['db'];

        $queries = $app->request->getQuery('queries', "int", 1);
        $queries = min(max(intval($queries), 1), 500);

        $worlds = [];

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $db->fetchOne('SELECT * FROM world WHERE id = ' . mt_rand(1, 10000), Enum::FETCH_ASSOC);
        }

        header("Content-Type: application/json");
        echo json_encode($worlds);
    });

    $app->map('/fortunes', function () use ($app) {
        $fortunes = $app['db']->query('SELECT * FROM fortune')->fetchAll();
        $fortunes[] = [
            'id' => 0,
            'message' => 'Additional fortune added at request time.'
        ];

        usort($fortunes, function ($left, $right) {
            return $left['message'] <=> $right['message'];
        });

        header("Content-Type: text/html; charset=utf-8");

        echo $app['view']->getRender('bench', 'fortunes', [
            'fortunes' => $fortunes,
        ]);
    });

    $url = $_REQUEST['_url'] ?? '/';
    $app->handle($url);
} catch (PhalconException $e) {
    echo "PhalconException: ", $e->getMessage();
}
