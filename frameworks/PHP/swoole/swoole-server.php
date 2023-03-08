<?php

use Swoole\Http\Server;
use Swoole\Http\Request;
use Swoole\Http\Response;

$server = new Swoole\Http\Server('0.0.0.0', 8080, SWOOLE_BASE);
$server->set([
    'worker_num' => swoole_cpu_num(),
    'log_file' => '/dev/null',
    'log_level' => 5,
]);

$pool = new \DatabasePool('postgres');

/**
 * On start of the PHP worker. One worker per server process is started.
 */
$server->on('workerStart', function ($srv) use ($pool) {
    $pool->init(\intdiv(512, $srv->setting['worker_num']));
});

/**
 * The DB test
 *
 * @param string $database_type
 * @param int $queries
 *
 * @return string
 */
$db_postgres = function (int $queries = 0) use ($pool): string {
    $db = $pool->get();
    // Read number of queries to run from URL parameter
    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    // Create an array with the response string.
    $arr = [];

    $db->s ??= $db->prepare('s', 'SELECT id, randomnumber FROM World WHERE id = $1');

    // For each query, store the result set values in the response array
    while ($query_count--) {
        $id = mt_rand(1, 10000);
        $res = $db->execute('s', [$id]);
        $ret = $db->fetchAssoc($res);
        // Store result in array.
        $arr[] = ['id' => $id, 'randomnumber' => $ret['randomnumber']];
    }

    // Use the PHP standard JSON encoder.
    // http://www.php.net/manual/en/function.json-encode.php
    if ($queries === -1) {
        $arr = $arr[0];
    }

    $pool->put($db);

    return \json_encode($arr, JSON_NUMERIC_CHECK);
};

/**
 * The Fortunes test
 *
 * @param string $database_type
 *
 * @return string
 */
$fortunes_postgres = function () use ($pool): string {
    $db = $pool->get();

    $fortune = [];

    $db->f ??= $db->prepare('f', 'SELECT id, message FROM Fortune');
    $res = $db->execute('f', []);
    $arr = $db->fetchAll($res);

    foreach ($arr as $row) {
        $fortune[$row['id']] = $row['message'];
    }
    $fortune[0] = 'Additional fortune added at request time.';
    \asort($fortune);

    $html = '';
    foreach ($fortune as $id => $message) {
        $message = \htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>{$id}</td><td>{$message}</td></tr>";
    }

    $pool->put($db);

    return '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
            .$html.
            '</table></body></html>';
};

/**
 * The Updates test
 *
 * @param string $database_type
 * @param int $queries
 *
 * @return string
 */
$updates_postgres = function (int $queries = 0) use ($pool): string {
    $db = $pool->get();

    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    $arr = [];

    $db->us ??= $db->prepare('us', 'SELECT id,randomnumber FROM World WHERE id = $1');
    $db->uu ??= $db->prepare('uu', 'UPDATE World SET randomnumber = $1 WHERE id = $2');

    while ($query_count--) {
        $id = \mt_rand(1, 10000);
        $randomNumber = \mt_rand(1, 10000);
        $res = $db->execute('us', [$id]);
        $ret = $db->fetchAssoc($res);
        // Store result in array.
        $world = ['id' => $id, 'randomnumber' => $ret['randomnumber']];
        $world['randomnumber'] = $randomNumber;
        $res = $db->execute('uu', [$randomNumber, $id]);
        $arr[] = $world;
    }

    $pool->put($db);

    return \json_encode($arr, JSON_NUMERIC_CHECK);
};

/**
 * The DB test
 *
 * @param string $database_type
 * @param int $queries
 *
 * @return string
 */
$db_mysql = function (int $queries = 0) use ($pool): string {
    $db = $pool->get();

    // Read number of queries to run from URL parameter
    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    // Create an array with the response string.
    $arr = [];
    // Define query
    $db->db_test ??= $db->prepare('SELECT id, randomNumber FROM World WHERE id = ?');

    // For each query, store the result set values in the response array
    while ($query_count--) {
        $id = \mt_rand(1, 10000);
        $ret = $db->db_test->execute([$id]);

        // Store result in array.
        $arr[] = ['id' => $id, 'randomNumber' => $ret[0]['randomNumber']];
    }

    // Use the PHP standard JSON encoder.
    // http://www.php.net/manual/en/function.json-encode.php
    if ($queries === -1) {
        $arr = $arr[0];
    }

    $pool->put($db);

    return \json_encode($arr, JSON_NUMERIC_CHECK);
};

/**
 * The Fortunes test
 *
 * @param string $database_type
 *
 * @return string
 */
$fortunes_mysql = function () use ($pool): string {
    $db = $pool->get();

    $fortune = [];

    $db->fortune_test ??= $db->prepare('SELECT id, message FROM Fortune');
    $arr = $db->fortune_test->execute();

    foreach ($arr as $row) {
        $fortune[$row['id']] = $row['message'];
    }
    $fortune[0] = 'Additional fortune added at request time.';
    \asort($fortune);

    $html = '';
    foreach ($fortune as $id => $message) {
        $message = \htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>{$id}</td><td>{$message}</td></tr>";
    }

    $pool->put($db);

    return '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
            .$html.
            '</table></body></html>';
};

/**
 * The Updates test
 *
 * @param string $database_type
 * @param int $queries
 *
 * @return string
 */
$updates_mysql = function (int $queries = 0) use ($pool): string {
    $db = $pool->get();

    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    $arr = [];
    $db->updates_test_select ??= $db->prepare('SELECT id,randomNumber FROM World WHERE id = ?');
    $db->updates_test_update ??= $db->prepare('UPDATE World SET randomNumber = ? WHERE id = ?');

    while ($query_count--) {
        $id = \mt_rand(1, 10000);
        $randomNumber = \mt_rand(1, 10000);
        $ret = $db->updates_test_select->execute([$id]);

        // Store result in array.
        $world = ['id' => $id, 'randomNumber' => $ret[0]['randomNumber']];
        $world['randomNumber'] = $randomNumber;
        $db->updates_test_update->execute([$randomNumber, $id]);

        $arr[] = $world;
    }

    $pool->put($db);

    return \json_encode($arr, JSON_NUMERIC_CHECK);
};


/**
 * On every request to the (web)server, execute the following code
 */
$server->on('request', function (Request $req, Response $res) use ($db, $fortunes, $updates) {
    try {
        switch ($req->server['request_uri']) {
            case '/json':
                $res->header('Content-Type', 'application/json');
                $res->end(json_encode(['message' => 'Hello, World!']));
                break;

            case '/plaintext':
                $res->header('Content-Type', 'text/plain; charset=utf-8');
                $res->end('Hello, World!');
                break;

            case '/db':
                $res->header('Content-Type', 'application/json');

                if (isset($req->get['queries'])) {
                    $res->end($db((int)$req->get['queries']));
                } else {
                    $res->end($db(-1));
                }
                break;

            case '/fortunes':
                $res->header('Content-Type', 'text/html; charset=utf-8');
                $res->end($fortunes());
                break;

            case '/updates':
                $res->header('Content-Type', 'application/json');

                if (isset($req->get['queries'])) {
                    $res->end($updates((int)$req->get['queries']));
                } else {
                    $res->end($updates(-1));
                }
                break;

            default:
                $res->status(404);
                $res->end('Error 404');

        }

    } catch (\Throwable $e) {
        $res->status(500);
        $res->end('Error 500');
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
    private $server = [
        'host' => '',
        'user' => 'benchmarkdbuser',
        'password' => 'benchmarkdbpass',
        'database' => 'hello_world'
    ];

    private $pool;

    private $type;

    public function __construct($type)
    {
        $this->server['host'] = \gethostbyname('tfb-database');
        $this->type = $type;
    }

    public function init($capacity)
    {
        $this->pool=new \Swoole\Coroutine\Channel($capacity);
        while($capacity>0){
            $db=$this->createDbInstance();
            if($db!==false){
                $this->pool->push($db);
                $capacity--;
            }
        }
    }

    private function createDbInstance()
    {
        if ($this->type === 'postgres') {
            $db = new Swoole\Coroutine\PostgreSql;
            if ($db->connect("host={$this->server['host']} port=5432 dbname={$this->server['database']} user={$this->server['user']} password={$this->server['password']}")){
                return $db;
            }
        } else if($this->type === 'mysql') {
            $db = new Swoole\Coroutine\Mysql;
            if ($db->connect($this->server)){
                return $db;
            }
        }

        return false;
    }

    public function put($db)
    {
        $this->pool->push($db);
    }

    public function get()
    {
        return $this->pool->pop();
    }
}
