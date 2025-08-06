<?php
require_once __DIR__ . '/vendor/autoload.php';


use Adapterman\Adapterman;
use Workerman\Worker;
use Workerman\Timer;

Adapterman::init();

$http_worker                = new Worker('http://0.0.0.0:8080');
$http_worker->count         = (int) shell_exec('nproc') * 4;
$http_worker->reusePort     = true;
$http_worker->name          = 'AdapterMan Phpixie';

$http_worker->onWorkerStart = static function () {
    HeaderDate::init();
    //chdir(__DIR__.'/public');
    require 'web/start.php';
};

$http_worker->onMessage = static function ($connection) {

    $connection->send(run());
};

Worker::runAll();

class HeaderDate
{
    const NAME = 'Date: ';

    /**
     * Date header
     *
     * @var string
     */
    public static $date;

    public static function init(): void
    {
        self::$date = self::NAME . gmdate(DATE_RFC7231);
        Timer::add(1, static function() {
            self::$date = self::NAME . gmdate(DATE_RFC7231);
        });
    }
}