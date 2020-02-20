<?php
require_once __DIR__.'/vendor/autoload.php';
require_once __DIR__.'/app.php';

use Workerman\Protocols\Http;
use Workerman\Worker;

$http_worker                = new Worker('http://0.0.0.0:8080');
$http_worker->count         = (int) shell_exec('nproc') * 4;
$http_worker->onWorkerStart = function () {
    init();
};

$http_worker->onMessage = static function ($connection) {

    Http::header('Date: '.gmdate('D, d M Y H:i:s').' GMT');

    $connection->send(router());
    
};

Worker::runAll();
