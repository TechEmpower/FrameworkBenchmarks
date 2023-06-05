<?php

require_once __DIR__.'/vendor/autoload.php';

use OpenSwoole\Http\Request;
use OpenSwoole\Http\Response;
use OpenSwoole\Http\Server;
use OpenSwoole\Util;
use OpenSwoole\Core\Coroutine\Client\PostgresClientFactory;
use OpenSwoole\Core\Coroutine\Client\PostgresConfig;
use OpenSwoole\Core\Coroutine\Pool\ClientPool;

$server = new Server('0.0.0.0', 8080, OpenSwoole\Server::SIMPLE_MODE);
$server->set([
    'worker_num' => Util::getCPUNum() * 2,
    'log_file' => '/dev/null',
    'log_level' => 5,
    'open_tcp_nodelay' => true,
]);

$pool = null;

/**
 * On start of the PHP worker. One worker per server process is started.
 */
$server->on('workerStart', function () use (&$pool) {
    $config = (new PostgresConfig())
        ->withHost('tfb-database')
        ->withDbname('hello_world')
        ->withUsername('benchmarkdbuser')
        ->withPassword('benchmarkdbpass');
    $pool   = new ClientPool(PostgresClientFactory::class, $config, \intdiv(512, Util::getCPUNum() * 2));
});

/**
 * The DB test
 *
 * @param string $database_type
 * @param int $queries
 *
 * @return string
 */
$db_postgres = function (int $queries = 0, $pool): string {
    $db = $pool->get();
    // Read number of queries to run from URL parameter
    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    // Create an array with the response string.
    $arr = [];

    $db->prepare('s', 'SELECT id, randomnumber FROM World WHERE id = $1');

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
$fortunes_postgres = function ($pool): string {
    $db = $pool->get();

    $fortune = [];

    $db->prepare('f', 'SELECT id, message FROM Fortune');
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
$updates_postgres = function (int $queries = 0, $pool): string {
    $db = $pool->get();

    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    $arr = [];

    $db->prepare('us', 'SELECT id,randomnumber FROM World WHERE id = $1');
    $db->prepare('uu', 'UPDATE World SET randomnumber = $1 WHERE id = $2');

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
 * On every request to the (web)server, execute the following code
 */
$server->on('request', function (Request $req, Response $res) use ($db_postgres, $fortunes_postgres, $updates_postgres, &$pool) {
    try {
        switch ($req->server['request_uri']) {
            case '/json':
                $res->header('Content-Type', 'application/json');
                $res->header('Server', 'os');
                $res->end(json_encode(['message' => 'Hello, World!']));
                break;

            case '/plaintext':
                $res->header('Content-Type', 'text/plain; charset=utf-8');
                $res->header('Server', 'os');
                $res->end('Hello, World!');
                break;

            case '/db':
                $res->header('Content-Type', 'application/json');
                $res->header('Server', 'os');
                if (isset($req->get['queries'])) {
                    $res->end($db_postgres((int)$req->get['queries'], $pool));
                } else {
                    $res->end($db_postgres(-1, $pool));
                }
                break;

            case '/fortunes':
                $res->header('Content-Type', 'text/html; charset=utf-8');
                $res->header('Server', 'os');
                $res->end($fortunes_postgres($pool));
                break;

            case '/updates':
                $res->header('Content-Type', 'application/json');
                $res->header('Server', 'os');
                if (isset($req->get['queries'])) {
                    $res->end($updates_postgres((int)$req->get['queries'], $pool));
                } else {
                    $res->end($updates_postgres(-1, $pool));
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
