<?php
require_once __DIR__ . '/vendor/autoload.php';

use Adapterman\Adapterman;
use Workerman\Worker;
use Workerman\Lib\Timer;

Adapterman::init();

require __DIR__.'/app/index.php';

$http_worker                = new Worker('http://0.0.0.0:8080');
$http_worker->count         = (int) shell_exec('nproc') * 4;
$http_worker->name          = 'AdapterMan-Yii2';

$http_worker->onWorkerStart = static function () {
    WorkerTimer::init();
};

$http_worker->onMessage = static function ($connection) {
 
    $_SERVER['SCRIPT_FILENAME'] = '/app/index.php';
    $_SERVER['SCRIPT_NAME'] = '/index.php';
    header(WorkerTimer::$date);
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
