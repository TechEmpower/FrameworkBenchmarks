<?php

try {

    $app = new Phalcon\Mvc\Micro();

    // Setting up the database connection
    $app['db'] = function() {

        return new \Phalcon\Db\Adapter\Pdo\Mysql(array(
            'host'       => 'localhost',
            'username'   => 'benchmarkdbuser',
            'password'   => 'benchmarkdbpass',
            'dbname'     => 'hello_world',
            'persistent' => true,
            'options'    => array(
                PDO::MYSQL_ATTR_INIT_COMMAND => 'SET NAMES utf8'
            )
        ));
    };

    // Setting up the view component (seems to be required even when not used)
    $app['view'] = function() {

        $view = new \Phalcon\Mvc\View();

        $view->setViewsDir(__DIR__ . '/../views/');

        $view->registerEngines(array(
            ".volt" => function($view, $di) {

                $volt = new \Phalcon\Mvc\View\Engine\Volt($view, $di);

                $volt->setOptions(array(
                    "compiledPath" => __DIR__ . "/../compiled-templates/",
                    "compiledExtension" => ".c",
                    "compiledSeparator" => '_',
                ));

                return $volt;
            }
        ));

        return $view;
    };

    $app->map('/json', function() {
        header("Content-Type: application/json");
        echo json_encode('Hello World!');
    });

    //
    $app->map('/db', function() use ($app) {

        $db = $app['db'];

        $queries = $app->request->getQuery('queries', null, 1);

        $worlds = array();

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $db->fetchOne('SELECT * FROM world WHERE id = ?', Phalcon\Db::FETCH_ASSOC, array(mt_rand(1, 10000)));
        }

        echo json_encode($worlds);
    });

    $app->map('/fortunes', function() use ($app) {

        // since the resultset is immutable get an array instead
        // so we can add the new fortune
        $fortunes = $app['db']->query('SELECT * FROM fortune')->fetchAll();

        $fortunes[] = array(
            'id' => 0,
            'message' => 'Additional fortune added at request time.'
        );

        usort($fortunes, function($left, $right) {
            $l = $left['message'];
            $r = $right['message'];
            if ($l === $r) {
                return 0;
            } else {
                if ($l > $r) {
                    return 1;
                } else {
                    return -1;
                }
            }
        });

        header("Content-Type: text/html; charset=utf-8");

        echo $app['view']->getRender('bench', 'fortunes', array(
            'fortunes' => $fortunes
        ));

    });

    $app->handle();

} catch(\Phalcon\Exception $e) {
    echo "PhalconException: ", $e->getMessage();
}
