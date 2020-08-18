<?php

try {

    $app = new Phalcon\Mvc\Micro();

    // Setting up the database connection
    $app['db'] = function() {

        return new \Phalcon\Db\Adapter\Pdo\Mysql(array(
            'host'       => 'tfb-database',
            'dbname'     =>  'hello_world',
            'username'   => 'benchmarkdbuser',
            'password'   => 'benchmarkdbpass',
            'options'    => [
                              PDO::MYSQL_ATTR_INIT_COMMAND => "SET NAMES 'UTF8'",
                              PDO::ATTR_PERSISTENT => TRUE,
                            ]
        ));
    };

    // Setting up the view component (seems to be required even when not used)
    $app['view'] = function() {

        $view = new \Phalcon\Mvc\View();

        $view->setViewsDir(__DIR__ . '/../app/views/');

        $view->registerEngines(array(
            ".volt" => function($view) {

                $volt = new \Phalcon\Mvc\View\Engine\Volt($view);

                $volt->setOptions(array(
                    "path" => __DIR__ . "/../app/compiled-templates/",
                    "extension" => ".c",
                    "separator" => '_',
                ));

                return $volt;
            }
        ));

        return $view;
    };
    
    $app->map('/plaintext', function() {
        header("Content-Type: text/plain; charset=UTF-8");
        echo "Hello, World!";       
    });

    $app->map('/json', function() {
        header("Content-Type: application/json");
        echo json_encode(array('message' => 'Hello, World!'));
    });

    //
    $app->map('/db', function() use ($app) {
        header("Content-Type: application/json");

        $db = $app['db'];
        
        $world = $db->fetchOne('SELECT * FROM world WHERE id = ' . mt_rand(1, 10000), Phalcon\Db\Enum::FETCH_ASSOC);

        echo json_encode($world);
    });


    // queries
    $app->map('/queries', function() use ($app) {
        header("Content-Type: application/json");

        $db = $app['db'];

        $queries = $app->request->getQuery('queries', null, 1);
        $queries = is_numeric($queries) ? min(max(intval($queries), 1), 500) : 1;

        $worlds = array();

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $db->fetchOne('SELECT * FROM world WHERE id = ' . mt_rand(1, 10000), Phalcon\Db\Enum::FETCH_ASSOC);
        }

        echo json_encode($worlds);
    });

    // /fortunes
    $app->map('/fortunes', function() use ($app) {

        $fortunes = $app['db']->query('SELECT * FROM fortune')->fetchAll();

        $fortunes[] = array(
            'id' => 0,
            'message' => 'Additional fortune added at request time.'
        );

        usort($fortunes, function($left, $right) {
            return $left['message'] <=> $right['message'];
        });

        header("Content-Type: text/html; charset=utf-8");

        echo $app['view']->getRender('bench', 'fortunes', array(
            'fortunes' => $fortunes
        ));

    });
    
    $url = $_REQUEST['_url'] ?? '/';
    $app->handle($url);

} catch(\Phalcon\Exception $e) {
    echo "PhalconException: ", $e->getMessage();
}
