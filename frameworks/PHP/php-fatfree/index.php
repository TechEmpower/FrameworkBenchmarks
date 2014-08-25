<?php
/** @var Base $f3 */
$f3=require('lib/base.php');

$f3->set('DEBUG',2);
$f3->set('CACHE','folder=tmp/cache/');
$f3->set('UI','ui/');

$f3->set('DBS',array('mysql:host=localhost;port=3306;dbname=hello_world','benchmarkdbuser','benchmarkdbpass'));

// http: //www.techempower.com/benchmarks/#section=code

// JSON test
$f3->route('GET /json',function($f3) {
    /** @var Base $f3 */
    header("Content-type: application/json");
    echo json_encode(array('message' => 'Hello, World!'));
});


// DB RAW test
$f3->route(
    array(
        'GET /db',                  // database-single-query
        'GET /db/@queries',         // database-multiple-queries
    ),
    function ($f3,$params) {
        /** @var Base $f3 */
        $single = !isset($params['queries']);
        if ($single)
            $queries = 1;
        else {
            $queries = (int) $params['queries'];
            $queries = ($queries < 1) ? 1 : (($queries > 500) ? 500 : $queries);
        }
        $dbc = $f3->get('DBS');
        $db = new \DB\SQL($dbc[0],$dbc[1],$dbc[2],array( \PDO::ATTR_PERSISTENT => TRUE ));
        $result = array();
        for ($i = 0; $i < $queries; $i++) {
            $id = mt_rand(1, 10000);
            $res = $db->exec('SELECT id, randomNumber FROM World WHERE id = ?',$id,0,false);
            $result[] = $res[0];
        }
        header("Content-type: application/json");
        echo json_encode($single ? $result[0] : $result);
    }
);

// DB ORM test
$f3->route(
    array(
         'GET /db-orm',             // database-single-query
         'GET /db-orm/@queries',    // database-multiple-queries
    ),
    function ($f3, $params) {
        /** @var Base $f3 */
        $single = !isset($params['queries']);
        if ($single)
            $queries = 1;
        else {
            $queries = (int) $params['queries'];
            $queries = ($queries < 1) ? 1 : (($queries > 500) ? 500 : $queries);
        }
        $dbc = $f3->get('DBS');
        $db = new \DB\SQL($dbc[0],$dbc[1],$dbc[2],array( \PDO::ATTR_PERSISTENT => TRUE ));
        $mapper = new \DB\SQL\Mapper($db,'World');
        $result = array();
        for ($i = 0; $i < $queries; $i++) {
            $id = mt_rand(1, 10000);
            $mapper->load(array('id = ?',$id));
            $result[] = $mapper->cast();
        }
        header("Content-type: application/json");
        echo json_encode($single ? $result[0] : $result);
    }
);


$f3->route('GET /plaintext', function ($f3) {
    echo "Hello, World!";
});


$f3->route('GET /fortune', function ($f3) {
    /** @var Base $f3 */
    $dbc = $f3->get('DBS');
    $db = new \DB\SQL($dbc[0],$dbc[1],$dbc[2],array( \PDO::ATTR_PERSISTENT => TRUE ));
    $result = $db->exec('SELECT id, message FROM Fortune');
    $result[] = array(
        'id'=>0,
        'message'=>'Additional fortune added at request time.'
    );
    $mtx = \Matrix::instance();
    $mtx->sort($result,'message');
    $f3->set('result',$result);
    echo \Template::instance()->render('fortune.html');
});


$f3->route('GET /update-raw/@queries', function($f3,$params) {
    /** @var Base $f3 */
    $queries = (int) $params['queries'];
    $queries = ($queries < 1) ? 1 : (($queries > 500) ? 500 : $queries);

    $dbc = $f3->get('DBS');
    $db = new \DB\SQL($dbc[0],$dbc[1],$dbc[2],array( \PDO::ATTR_PERSISTENT => TRUE ));

    $result = array();
    for ($i = 0; $i < $queries; $i++) {
        $id = mt_rand(1, 10000);
        $row = array(
            'id'=>$id,
            'randomNumber'=>$db->exec('SELECT randomNumber FROM World WHERE id = ?',$id,0,false)
        );
        $rnu = mt_rand(1, 10000);
        $row['randomNumber'] = $rnu;
        $db->exec('UPDATE World SET randomNumber = :ranNum WHERE id = :id', array(':ranNum'=>$rnu,':id'=>$id),0,false);
        $result[] = $row;
    }

    header("Content-type: application/json");
    echo json_encode($result);

});


$f3->route('GET /update-orm/@queries', function($f3,$params) {
    /** @var Base $f3 */
    $queries = (int) $params['queries'];
    $queries = ($queries < 1) ? 1 : (($queries > 500) ? 500 : $queries);

    $dbc = $f3->get('DBS');
    $db = new \DB\SQL($dbc[0],$dbc[1],$dbc[2],array( \PDO::ATTR_PERSISTENT => TRUE ));
    $world = new \DB\SQL\Mapper($db,'World');

    $result = array();
    for ($i = 0; $i < $queries; $i++) {
        $id = mt_rand(1, 10000);
        $world->load(array('id = ?', $id));
        $world->randomNumber = mt_rand(1, 10000);
        $world->save();
        $result[] = $world->cast();
    }
    header("Content-type: application/json");
    echo json_encode($result);

});

$f3->run();
