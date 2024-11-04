<?php
require_once __DIR__.'/vendor/autoload.php';

use Workerman\Worker;
use Workerman\Events\Swow;
use Workerman\Events\Swoole;
use Workerman\Events\Select;
use Workerman\Protocols\Http\Response;

$test_type = getenv('TEST_TYPE') ?: 'default';
$process = getenv('PROCESS_MULTIPLIER') ?: 1;
$pool_size = getenv('POOL_SIZE') ?: 2;
$process_count = (int) shell_exec('nproc') * $process;

$db = $date = null;
$http_worker = new Worker('http://0.0.0.0:8080');
//$http_worker->reusePort = true;
$http_worker->count = $process_count;
$http_worker->onWorkerStart = static function () use ($test_type, $pool_size, &$db, &$date) {
    $db = match ($test_type) {
        'pgsql' => new Pgsql(),
        'mysql' => new Mysql(),
        'pgsql-swow' => new PgsqlSwow($pool_size),
        'mysql-swow' => new MysqlSwow($pool_size),
        'pgsql-swoole' => new PgsqlSwoole($pool_size),
        'mysql-swoole' => new MysqlSwoole($pool_size),
        'default' => new Mysql(),
    };
    $date = new Date();
};
if ($test_type === 'default') {
    Worker::$eventLoopClass = Select::class;
} elseif (in_array($test_type, ['pgsql-swow', 'mysql-swow'])) {
    Worker::$eventLoopClass = Swow::class;
} elseif (in_array($test_type, ['pgsql-swoole', 'mysql-swoole'])) {
    Worker::$eventLoopClass = Swoole::class;
}

$http_worker->onMessage = static function ($connection, $request) use (&$db, &$date) {
    switch ($request->path()) {
        case '/plaintext':
            $connection->headers = [
                'Content-Type' => 'text/plain',
                'Date' => $date->date
            ];
            return $connection->send('Hello, World!');
        case '/json':
            $connection->headers = [
                'Content-Type' => 'application/json',
                'Date' => $date->date
            ];
            return $connection->send(json_encode(['message' => 'Hello, World!']));
        case '/db':
            $connection->headers = [
                'Content-Type' => 'application/json',
                'Date' => $date->date
            ];
            return $connection->send(json_encode($db->db()));
        case '/fortunes':
            $connection->headers = [
                'Date' => $date->date
            ];
            return $connection->send($db->fortune());
        case '/query':
            $connection->headers = [
                'Content-Type' => 'application/json',
                'Date' => $date->date
            ];
            return $connection->send(json_encode($db->query($request)));
        case '/update':
            $connection->headers = [
                'Content-Type' => 'application/json',
                'Date' => $date->date
            ];
            return $connection->send(json_encode($db->update($request)));
    }
    return $connection->send(new Response(404, [
        'Content-Type' => 'text/plain',
        'Date' => $date->date
    ], '404 Not Found'));
};

Worker::runAll();
