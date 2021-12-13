<?php
// drop startup errors
// if (function_exists('error_clear_last'))
//     error_clear_last();

// error_reporting(0);
require_once 'vendor/autoload.php';

/** @var Base $f3 */
$f3 = \Base::instance();

error_reporting(-1);

$f3->set('DEBUG', 0);
$f3->set('HIGHLIGHT', false);
$f3->set('CACHE', 'folder=tmp/cache/');
$f3->set('UI', 'ui/');
$f3->set('ONERROR', function ($f3) {
    echo $f3->get('ERROR.code') . ': ' . $f3->get('ERROR.text') . "\n" . $f3->get('ERROR.trace');
});

$f3->set('DBS', array('mysql:host=tfb-database;port=3306;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', [\PDO::ATTR_PERSISTENT => TRUE]));
// http: //www.techempower.com/benchmarks/#section=code

// JSON test
$f3->route('GET /json', function ($f3) {
    /** @var Base $f3 */
    header('Content-type: application/json');
    echo json_encode(['message' => 'Hello, World!']);
});


// DB RAW test database-single-query
$f3->route(
    'GET /db',
    function ($f3) {
        /** @var Base $f3 */
        $db = new \DB\SQL(...$f3->get('DBS'));
        $id = mt_rand(1, 10000);
        $res = $db->exec('SELECT id, randomNumber FROM World WHERE id = ?', $id, 0, false);
        $result = array(
            'id' => (int) $res[0]['id'],
            'randomNumber' => (int) $res[0]['randomNumber'],
        );
        header('Content-type: application/json');
        echo json_encode($result);
    }
);

// DB RAW test database-multiple-query
$f3->route(
    array(
        'GET /db-multiple',
        'GET /db-multiple/',
        'GET /db-multiple/@queries',
    ),
    function ($f3, $params) {
        /** @var Base $f3 */
        $queries = 1;
        if (isset($params['queries'])) {
            $queries = (int) $params['queries'];
            $queries = ($queries < 1) ? 1 : (($queries > 500) ? 500 : $queries);
        }
        $db = new \DB\SQL(...$f3->get('DBS'));
        $result = array();
        for ($i = 0; $i < $queries; ++$i) {
            $id = mt_rand(1, 10000);
            $res = $db->exec('SELECT id, randomNumber FROM World WHERE id = ?', $id, 0, false);
            $result[] = array(
                'id' => (int) $res[0]['id'],
                'randomNumber' => (int) $res[0]['randomNumber'],
            );
        }
        header('Content-type: application/json');
        echo json_encode($result);
    }
);

// DB ORM test database-single-query
$f3->route(
    'GET /db-orm',
    function ($f3) {
        /** @var Base $f3 */
        $db = new \DB\SQL(...$f3->get('DBS'));
        $mapper = new \DB\SQL\Mapper($db, 'World');
        $id = mt_rand(1, 10000);
        $mapper->load(array('id = ?', $id));
        header('Content-type: application/json');
        echo json_encode($mapper->cast());
    }
);

// DB ORM test database-multiple-queries
$f3->route(
    array(
        'GET /db-orm-multiple',
        'GET /db-orm-multiple/',
        'GET /db-orm-multiple/@queries',    // database-multiple-queries
    ),
    function ($f3, $params) {
        /** @var Base $f3 */
        $queries = 1;
        if (isset($params['queries'])) {
            $queries = (int) $params['queries'];
            $queries = ($queries < 1) ? 1 : (($queries > 500) ? 500 : $queries);
        }
        $db = new \DB\SQL(...$f3->get('DBS'));
        $mapper = new \DB\SQL\Mapper($db, 'World');
        $result = array();
        for ($i = 0; $i < $queries; ++$i) {
            $id = mt_rand(1, 10000);
            $mapper->load(array('id = ?', $id));
            $result[] = $mapper->cast();
        }
        header('Content-type: application/json');
        echo json_encode($result);
    }
);


$f3->route('GET /plaintext', function () {
    header('Content-type: text/plain');
    echo 'Hello, World!';
});


$f3->route('GET /fortune-orm', function ($f3) {
    /** @var Base $f3 */
    $db = new \DB\SQL(...$f3->get('DBS'));
    $mapper = new \DB\SQL\Mapper($db, 'Fortune');
    $result = $mapper->find();
    //$result = $db->exec('SELECT id, message FROM Fortune');
    $result[] = [
        'id' => 0,
        'message' => 'Additional fortune added at request time.'
    ];
    $mtx = \Matrix::instance();
    $mtx->sort($result, 'message');
    $f3->set('result', $result);
    echo \Template::instance()->render('fortune.html');
});

$f3->route('GET /fortune-raw', function ($f3) {
    /** @var Base $f3 */
    $db = new \DB\SQL(...$f3->get('DBS'));
    $result = $db->exec('SELECT id, message FROM Fortune');
    $result[] = array(
        'id' => 0,
        'message' => 'Additional fortune added at request time.'
    );
    $mtx = \Matrix::instance();
    $mtx->sort($result, 'message');
    $f3->set('result', $result);
    echo \Template::instance()->render('fortune.html');
});


$f3->route(array(
    'GET /update-raw',
    'GET /update-raw/',
    'GET /update-raw/@queries'
), function ($f3, $params) {
    /** @var Base $f3 */
    $queries = 1;
    if (isset($params['queries'])) {
        $queries = (int) $params['queries'];
        $queries = ($queries < 1) ? 1 : (($queries > 500) ? 500 : $queries);
    }
    $db = new \DB\SQL(...$f3->get('DBS'));

    $result = array();
    for ($i = 0; $i < $queries; ++$i) {
        $id = mt_rand(1, 10000);
        $row = array(
            'id' => $id,
            'randomNumber' => $db->exec('SELECT id, randomNumber FROM World WHERE id = ?', $id, 0, false)
        );
        $rnu = mt_rand(1, 10000);
        $row['randomNumber'] = $rnu;
        $db->exec('UPDATE World SET randomNumber = :ranNum WHERE id = :id', array(':ranNum' => $rnu, ':id' => $id), 0, false);
        $result[] = $row;
    }
    header('Content-type: application/json');
    echo json_encode($result);
});


$f3->route(array(
    'GET /update-orm',
    'GET /update-orm/',
    'GET /update-orm/@queries'
), function ($f3, $params) {
    /** @var Base $f3 */
    $queries = 1;
    if (isset($params['queries'])) {
        $queries = (int) $params['queries'];
        $queries = ($queries < 1) ? 1 : (($queries > 500) ? 500 : $queries);
    }
    $db = new \DB\SQL(...$f3->get('DBS'));
    $world = new \DB\SQL\Mapper($db, 'World');

    $result = array();
    for ($i = 0; $i < $queries; ++$i) {
        $id = mt_rand(1, 10000);
        $world->load(array('id = ?', $id));
        $world->randomNumber = mt_rand(1, 10000);
        $world->save();
        $result[] = $world->cast();
    }
    header('Content-type: application/json');
    echo json_encode($result);
});

$f3->run();
