<?php
require_once __DIR__.'/vendor/autoload.php';

use Workerman\Worker;
use Workerman\Timer;

$test_type = getenv('TEST_TYPE') ?: 'default';
if ($test_type === 'pgsql') {
    require_once __DIR__.'/app-pg.php';
} else {
    require_once __DIR__.'/app.php';
}
$process_count = (int) shell_exec('nproc') * ($test_type === 'default' ? 1 : 4);

$http_worker                = new Worker('http://0.0.0.0:8080');
$http_worker->reusePort = true;
$http_worker->count         = $process_count;
$http_worker->onWorkerStart = static function () use ($test_type) {
    Header::$date = gmdate('D, d M Y H:i:s').' GMT';
    Timer::add(1, function() {
        Header::$date = gmdate('D, d M Y H:i:s').' GMT';
    });
    if ($test_type === 'pgsql') {
        DbRaw::init();
    } else {
        init();
    }
};

$http_worker->onMessage = static function ($connection, $request) {
    $connection->send(router($request));
};

Worker::runAll();

class Header {
    public static $date = null;
}
