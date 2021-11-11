<?php
require_once __DIR__.'/vendor/autoload.php';
require_once __DIR__.'/app.php';

use Workerman\Worker;
use Workerman\Timer;

$http_worker                = new Worker('http://0.0.0.0:8080');
$http_worker->count         = (int) shell_exec('nproc') * 4;
$http_worker->onWorkerStart = function () {
    Header::$date = gmdate('D, d M Y H:i:s').' GMT';
    Timer::add(1, function() {
        Header::$date = gmdate('D, d M Y H:i:s').' GMT';
    });
    init();
};

$http_worker->onMessage = static function ($connection, $request) {

    $connection->send(router($request));
    
};

Worker::runAll();


class Header {
    public static $date = null;
}
