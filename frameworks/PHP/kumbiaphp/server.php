<?php
require_once __DIR__.'/vendor/autoload.php';
require_once __DIR__.'/bench/app/workerbootstrap.php';

use Adapterman\Adapterman;
use Workerman\Worker;

Adapterman::init();

$http_worker            = new Worker('http://0.0.0.0:8080');
$http_worker->count     = (int) shell_exec('nproc') * 4;
$http_worker->name      = 'KumbiaPHP';

$http_worker->onWorkerStart = static function () {
    kumbiaInit();
    KuRaw::init();
};

$http_worker->onMessage = static function ($connection) {

    $connection->send(kumbiaSend());
};

Worker::runAll();
