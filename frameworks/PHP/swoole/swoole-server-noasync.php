<?php

use Swoole\Http\Request;
use Swoole\Http\Response;

$server = new swoole_http_server('0.0.0.0', 8080, SWOOLE_BASE);
$server->set([
    'worker_num' => swoole_cpu_num()
]);

$pdo = new PDO("mysql:host=tfb-database;dbname=hello_world", "benchmarkdbuser", "benchmarkdbpass", [
    PDO::ATTR_PERSISTENT => true
]);

/**
 * The DB test
 *
 * @param int $queries
 *
 * @return string
 */
$db = function (int $queries = 1) use ($pdo): string {
    if ( $queries === -1) {
        $statement = $pdo->prepare("SELECT id,randomNumber FROM World WHERE id=?");
        $statement->execute([mt_rand(1, 10000)]);
        return json_encode($statement->fetch(PDO::FETCH_ASSOC), JSON_NUMERIC_CHECK);
    }
    
    // Read number of queries to run from URL parameter
    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    // Create an array with the response string.
    $arr = [];
    // Define query
    $db = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id = ?');

    // For each query, store the result set values in the response array
    while ($query_count--) {
        $db->execute([mt_rand(1, 10000)]);
        $arr[] = $db->fetch(PDO::FETCH_ASSOC);
    }

    // Use the PHP standard JSON encoder.
    // http://www.php.net/manual/en/function.json-encode.php

    return json_encode($arr, JSON_NUMERIC_CHECK);
};

/**
 * The Fortunes test
 *
 * @return string
 */
$fortunes = function () use ($pdo): string {

    $fortune = [];
    $db = $pdo->prepare('SELECT id, message FROM Fortune');
    $db->execute();
    $fortune = $db->fetchAll(PDO::FETCH_KEY_PAIR);

    $fortune[0] = 'Additional fortune added at request time.';
    asort($fortune);

    $html = '';
    foreach ($fortune as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    return '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'. 
            $html.
            '</table></body></html>';
};

/**
 * The Updates test
 *
 * @param int $queries
 *
 * @return string
 */
$updates = function (int $queries) use ($pdo): string {
    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    $statement       = $pdo->prepare("SELECT randomNumber FROM World WHERE id=?");
    $updateStatement = $pdo->prepare("UPDATE World SET randomNumber=? WHERE id=?");

    while ($query_count--) {
        $id = mt_rand(1, 10000);
        $statement->execute([$id]);

        $world = ["id" => $id, "randomNumber" => $statement->fetchColumn()];
        $updateStatement->execute(
            [$world["randomNumber"] = mt_rand(1, 10000), $id]
        );

        $arr[] = $world;
    }


    return json_encode($arr, JSON_NUMERIC_CHECK);
};

/**
 * On start of the PHP worker. One worker per server process is started.
 */
//$server->on('workerStart', function () use ($pool) {
//});

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
                    $res->end($db((int) $req->get['queries']));
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
                $res->end($updates((int) $req->get['queries'] ?? 1));
                break;
        }

    } catch (\Throwable $e) {
        $res->status(500);
        $res->end('code ' . $e->getCode(). 'msg: '. $e->getMessage());
    }
});

$server->start();
