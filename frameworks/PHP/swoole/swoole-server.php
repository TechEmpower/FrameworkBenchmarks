<?php
$server = new swoole_http_server('0.0.0.0', 8080, SWOOLE_BASE);
$server->set(array(
    'worker_num' => NUMCORES
));

$pool = new MySQLPool();

$server->on('workerStart', function () use (&$pool) {
    $pool->set_host_ip();
});

$server->on('request', function ($req, $res) use ($pool) {

    switch ($req->server['request_uri'])
    {
        case "/json":
            $res->header('Content-Type', 'application/json');
            $res->end(json_encode(array('message' => 'Hello, World!')));
            break;

        case "/plaintext":
            $res->header('Content-Type', 'text/plain; charset=utf-8');
            $res->end('Hello, World!');
            break;

        case "/db":
            $db = $pool->get();

            // Read number of queries to run from URL parameter
            $query_count = 1;
            if (isset($req->get['queries']) && $req->get['queries'] > 0)
                $query_count = $req->get['queries'] > 500 ? 500 : $req->get['queries'];

            // Create an array with the response string.
            $arr = array();
            // Define query
            $stmt = $db->prepare('SELECT randomNumber FROM World WHERE id = ?');

            // For each query, store the result set values in the response array
            while (0 < $query_count--)
            {
                $id = mt_rand(1, 10000);
                $ret = $stmt->execute(array($id));

                // Store result in array.
                $arr[] = array('id' => $id, 'randomNumber' => $ret[0]['randomNumber']);
            }

            // Use the PHP standard JSON encoder.
            // http://www.php.net/manual/en/function.json-encode.php
            if (count($arr) === 1)
                $arr = $arr[0];

            $pool->put($db);

            $res->header('Content-Type', 'application/json');
            $res->end(json_encode($arr));
            break;

        case "/fortunes":
            $db = $pool->get();

            $fortune = [];
            // Define query
            $arr = $db->query('SELECT id, message FROM Fortune');
            foreach ($arr as $row)
                $fortune[$row['id']] = $row['message'];
            $fortune[0] = 'Additional fortune added at request time.';
            asort($fortune);

            $html = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
            foreach ($fortune as $id => $message)
                $html .= "<tr><td>" . $id . "</td><td>" . htmlspecialchars($message, ENT_QUOTES, 'UTF-8') . "</td></tr>";

            $html .= "</table></body></html>";

            $pool->put($db);

            $res->header('Content-Type', 'text/html; charset=utf-8');
            $res->end($html);
            break;

        case "/updates":
            $db = $pool->get();

            $query_count = 1;
            if (isset($req->get['queries']) && $req->get['queries'] > 0)
                $query_count = $req->get['queries'] > 500 ? 500 : $req->get['queries'];

            $arr = array();
            $statement = $db->prepare('SELECT randomNumber FROM World WHERE id = ?');
            $updateStatement = $db->prepare('UPDATE World SET randomNumber = ? WHERE id = ?');

            while (0 < $query_count--)
            {
                $id = mt_rand(1, 10000);
                $randomNumber = mt_rand(1, 10000);
                $ret = $statement->execute(array($id));

                // Store result in array.
                $world = array('id' => $id, 'randomNumber' => $ret[0]['randomNumber']);
                $world['randomNumber'] = $randomNumber;
                $updateStatement->execute(array($randomNumber, $id));

                $arr[] = $world;
            }

            $pool->put($db);

            $res->header('Content-Type', 'application/json');
            $res->end(json_encode($arr));
            break;
    }

});

$server->start();

/**
 * Class MySQLPool
 *
 * Deal with the fact that Swoole 2.1.3 has no build in database pooling
 */
class MySQLPool
{
    protected $pool;
    var $pool_count = 0;

    var $server = [
        'host' => '',
        'user' => 'benchmarkdbuser',
        'password' => 'benchmarkdbpass',
        'database' => 'hello_world'
    ];

    function __construct()
    {
        $this->pool = new \SplQueue;
    }

    function set_host_ip()
    {
        if( empty( $this->server['host'] ) )
        {
            $tfb_database_ip = Swoole\Coroutine::gethostbyname('tfb-database');
            $this->server['host'] = $tfb_database_ip;
        }
    }

    function put($db)
    {
        $this->pool->push($db);
        $this->pool_count++;
    }

    function get()
    {
        if($this->pool_count > 0)
        {
            $this->pool_count--;
            return $this->pool->pop();
        }

        // No idle connection to create a new connection
        $db = new Swoole\Coroutine\Mysql;
        $db->connect($this->server);

        if ($db == false)
            return false;
        else
            return $db;
    }
}
