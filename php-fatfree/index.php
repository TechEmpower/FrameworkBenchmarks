<?php
/** @var Base $f3 */
$f3=require('lib/base.php');

$f3->set('DEBUG',0);
$f3->set('CACHE',true);
$f3->set('UI','ui/');
// lazy initialized DB object
$f3->set('DB',function() {
	return new \DB\SQL('mysql:host=localhost;port=3306;dbname=hello_world',
	                   'benchmarkdbuser', 'benchmarkdbpass');
});

// https://github.com/TechEmpower/FrameworkBenchmarks#json-response
$f3->route('GET /json',function($f3) {
    /** @var Base $f3 */
    header("Content-type: application/json");
    return json_encode(array('message' => 'Hello World!'));
});


// https://github.com/TechEmpower/FrameworkBenchmarks#database-single-query
// https://github.com/TechEmpower/FrameworkBenchmarks#database-multiple-queries
$f3->route(
    array(
        'GET /db',
        'GET /db/@queries',
    ),
    function ($f3,$params) {
    /** @var Base $f3 */
        $params += array('queries' => 1); //default value        
        $dbc = $f3->get('DB');
        $db = $dbc();
        $result = array();
        for ($i = 0; $i < $params['queries']; ++$i) {
            $id = mt_rand(1, 10000);
            $result[] = $db->exec('SELECT randomNumber FROM World WHERE id = ?',$id,0,false);
        }

        header("Content-type: application/json");
        return json_encode($result);
    }
);

// https://github.com/TechEmpower/FrameworkBenchmarks#database-single-query
// https://github.com/TechEmpower/FrameworkBenchmarks#database-multiple-queries
$f3->route(
    array(
         'GET /db-orm',
         'GET /db-orm/@queries',
    ),
    function ($f3, $params) {
        /** @var Base $f3 */
        $params += array('queries' => 1); //default value        
        $dbc = $f3->get('DB');
        $db = $dbc();
        $mapper = new \DB\SQL\Mapper($db,'World');
        $result = array();
        for ($i = 0; $i < $params['queries']; ++$i) {
            $id = mt_rand(1, 10000);
            $mapper->load(array('where id = ?',$id));
            $result[] = $mapper->cast();
        }

        header("Content-type: application/json");
        return json_encode($result);
    }
);


$f3->route('GET /plaintext', function ($f3) {
    echo "Hello, World!";
});


$f3->route('GET /fortune', function ($f3) {
    /** @var Base $f3 */
    $dbc = $f3->get('DB');
    $db = $dbc();
    $result = $db->exec('SELECT id, message FROM Fortune');
    $result[] = 'Additional fortune added at request time.';
    asort($result);
    $f3->set('result',$result);
    echo \Template::instance()->render('fortune.html');
});


$f3->route(
    array(
         'GET /updateraw',
         'GET /updateraw/@queries',
    ),function($f3,$params) {
    /** @var Base $f3 */
    $params += array('queries' => 1); //default value    
    $dbc = $f3->get('DB');
    $db = $dbc();
    
    $result = array();
    for ($i = 0; $i < $params['queries']; ++$i) {
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
    return json_encode($result);
        
});

$f3->run();
