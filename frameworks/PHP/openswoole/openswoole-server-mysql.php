<?php

require_once __DIR__.'/vendor/autoload.php';

use OpenSwoole\Http\Request;
use OpenSwoole\Http\Response;
use OpenSwoole\Http\Server;
use OpenSwoole\Util;
use OpenSwoole\Core\Coroutine\Client\PDOClientFactory;
use OpenSwoole\Core\Coroutine\Client\PDOConfig;
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
$server->on('workerStart', function () use(&$pool) {
    $config = (new PDOConfig())
        ->withHost('tfb-database')
        ->withDbname('hello_world')
        ->withUsername('benchmarkdbuser')
        ->withPassword('benchmarkdbpass')
        ->withOptions([
            PDO::ATTR_DEFAULT_FETCH_MODE  => PDO::FETCH_ASSOC,
            PDO::ATTR_ERRMODE             => PDO::ERRMODE_EXCEPTION,
            PDO::ATTR_EMULATE_PREPARES    => false
        ]);
    $pool   = new ClientPool(PDOClientFactory::class, $config, \intdiv(512, Util::getCPUNum() * 2));
});

/**
 * The DB test
 *
 * @param string $database_type
 * @param int $queries
 *
 * @return string
 */
$db_mysql = function (int $queries, $pool): string {
    $db = $pool->get();

    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    // Create an array with the response string.
    $arr = [];

    $db_test = $db->prepare('SELECT id, randomNumber FROM World WHERE id = ?');

    // For each query, store the result set values in the response array
    while ($query_count--) {
        $db_test->execute([\mt_rand(1, 10000)]);
        $arr[] = $db_test->fetch();
    }
    $pool->put($db);

    if($queries < 0) {
        return \json_encode($arr[0], JSON_NUMERIC_CHECK);
    }

    return \json_encode($arr, JSON_NUMERIC_CHECK);
};

/**
 * The Fortunes test
 *
 * @param string $database_type
 *
 * @return string
 */
$fortunes_mysql = function ($pool): string {
    $db = $pool->get();

    $fortune = [];

    $fortune_test = $db->prepare('SELECT id, message FROM Fortune');
    $fortune_test->execute();
    $fortune = $fortune_test->fetchAll(PDO::FETCH_KEY_PAIR);

    $fortune[0] = 'Additional fortune added at request time.';
    asort($fortune);

    $html = '';
    foreach ($fortune as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    $pool->put($db);

    return "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>";
};

/**
 * The Updates test
 *
 * @param string $database_type
 * @param int $queries
 *
 * @return string
 */
$updates_mysql = function (int $queries = 0, $pool): string {
    $db = $pool->get();

    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    $arr = [];
    $updates_test_select = $db->prepare('SELECT id,randomNumber FROM World WHERE id = ?');
    $updates_test_update = $db->prepare('UPDATE World SET randomNumber = ? WHERE id = ?');

    while ($query_count--) {
        $id = \mt_rand(1, 10000);
        $updates_test_select->execute([$id]);
        $world = ["id" => $id, "randomNumber" => $updates_test_select->fetchColumn()];
        $world['randomNumber'] = mt_rand(1, 10000);
        $updates_test_update->execute([$world['randomNumber'], $world['id']]);
        $arr[] = $world;
    }

    $pool->put($db);

    return \json_encode($arr, JSON_NUMERIC_CHECK);
};


/**
 * On every request to the (web)server, execute the following code
 */
$server->on('request', static function (Request $req, Response $res) use ($db_mysql, $db_query, $fortunes_mysql, $updates_mysql, &$pool) {
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
                    $res->end($db_mysql((int)$req->get['queries'], $pool));
                } else {
                    $res->end($db_mysql(-1, $pool));
                }
                break;

            case '/fortunes':
                $res->header('Content-Type', 'text/html; charset=utf-8');
                $res->header('Server', 'os');
                $res->end($fortunes_mysql($pool));
                break;

            case '/updates':
                $res->header('Content-Type', 'application/json');
                $res->header('Server', 'os');
                if (isset($req->get['queries'])) {
                    $res->end($updates_mysql((int)$req->get['queries'], $pool));
                } else {
                    $res->end($updates_mysql(-1, $pool));
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
