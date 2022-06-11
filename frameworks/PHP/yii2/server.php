<?php
require_once __DIR__ . '/vendor/autoload.php';

use Adapterman\Adapterman;
use Workerman\Worker;
use Workerman\Lib\Timer;
use Workerman\Protocols\Http;

Adapterman::init();

require_once __DIR__.'/app/index.php';

$http_worker                = new Worker('http://0.0.0.0:8080');
$http_worker->count         = (int) shell_exec('nproc') * 4;
$http_worker->name          = 'AdapterMan';
$http_worker->onWorkerStart = function () {
    WorkerTimer::init();
    //init();
};

$http_worker->onMessage = static function ($connection, $request) {
 
    $_SERVER['SCRIPT_FILENAME'] = '/app/index.php';
    $_SERVER['SCRIPT_NAME'] = '/index.php';
    Http::header(WorkerTimer::$date);
    $connection->send(
        handleWorkerman()
    );
};

class WorkerTimer
{
    public static $date;

    public static function init()
    {
        self::$date = 'Date: '.gmdate('D, d M Y H:i:s').' GMT';
        Timer::add(1, function() {
            WorkerTimer::$date = 'Date: '.gmdate('D, d M Y H:i:s').' GMT';
        });
    }
}

Worker::runAll();