<?php
require_once __DIR__.'/vendor/autoload.php';

use Adapterman\Adapterman;
use Workerman\Lib\Timer;
use Workerman\Worker;

Adapterman::init();

$http_worker        = new Worker('http://0.0.0.0:8080');
$http_worker->count = (int) shell_exec('nproc') * 4;
$http_worker->name  = 'AdapterMan-CakePHP';

$http_worker->onWorkerStart = static function () {
    HeaderDate::init();
    require __DIR__.'/start.php';
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
        self::$date = self::NAME.gmdate('D, d M Y H:i:s').' GMT';
        Timer::add(1, static function () {
            self::$date = self::NAME.gmdate('D, d M Y H:i:s').' GMT';
        });
    }
}
