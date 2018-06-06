<?php
$server = new swoole_http_server('0.0.0.0', 8080, SWOOLE_BASE);
$server->set(array(
    'worker_num' => NUMCORES
));

$pool = new DatabasePool();

/**
 * The DB test
 *
 * @param string $database_type
 * @param int    $queries
 *
 * @return string
 */
$db = function ($database_type, $queries) use ($pool) {
    $db = $pool->get($database_type);

    // Read number of queries to run from URL parameter
    $query_count = 1;
    if ($queries > 0)
        $query_count = $queries > 500 ? 500 : $queries;

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

    return json_encode($arr);
};

/**
 * The Fortunes test
 *
 * @param string $database_type
 *
 * @return string
 */
$fortunes = function ($database_type) use ($pool) {
    $db = $pool->get($database_type);

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

    return $html;
};

/**
 * The Updates test
 *
 * @param string $database_type
 * @param int    $queries
 *
 * @return string
 */
$updates = function ($database_type, $queries) use ($pool) {
    $db = $pool->get($database_type);

    $query_count = 1;
    if ($queries > 0)
        $query_count = $queries > 500 ? 500 : $queries;

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

    return json_encode($arr);
};

/**
 * On start of the PHP worker. One worker per server process is started.
 */
$server->on('workerStart', function () use (&$pool) {
    $pool->set_host_ip();
});

/**
 * On every request to the (web)server, execute the following code
 */
$server->on('request', function ($req, $res) use ($pool, $db, $fortunes, $updates) {

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
            $res->header('Content-Type', 'application/json');

            if (isset($req->get['queries']) && $req->get['queries'] > 0)
                $res->end($db("mysql", $req->get['queries']));
            else
                $res->end($db("mysql", 0));
            break;

        case "/fortunes":
            $res->header('Content-Type', 'text/html; charset=utf-8');
            $res->end($fortunes("mysql"));
            break;

        case "/updates":
            $res->header('Content-Type', 'application/json');

            if (isset($req->get['queries']) && $req->get['queries'] > 0)
                $res->end($updates("mysql", $req->get['queries']));
            else
                $res->end($updates("mysql", 0));
            break;

        case "/db_postgres":
            $res->header('Content-Type', 'application/json');

            if (isset($req->get['queries']) && $req->get['queries'] > 0)
                $res->end($db("postgres", $req->get['queries']));
            else
                $res->end($db("postgres", 0));
            break;

        case "/fortunes_postgres":
            $res->header('Content-Type', 'text/html; charset=utf-8');
            $res->end($fortunes("postgres"));
            break;

        case "/updates_postgres":
            $res->header('Content-Type', 'application/json');

            if (isset($req->get['queries']) && $req->get['queries'] > 0)
                $res->end($updates("postgres", $req->get['queries']));
            else
                $res->end($updates("postgres", 0));
            break;
    }

});

$server->start();

/**
 * Class DatabasePool
 *
 * Deal with the fact that Swoole 2.1.3 has no build in database pooling
 */
class DatabasePool
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
        if (empty($this->server['host']))
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

    function get($server_type)
    {
        if ($this->pool_count > 0)
        {
            $this->pool_count--;
            return $this->pool->pop();
        }

        // No idle connection, time to create a new connection
        if( $server_type == "mysql" )
            $db = new Swoole\Coroutine\Mysql;

        if( $server_type == "postgres" )
            $db = new Swoole\Coroutine\PostgreSql;

        $db->connect($this->server);

        if ($db == false)
            return false;

        return $db;
    }
}
