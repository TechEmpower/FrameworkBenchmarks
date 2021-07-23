<?php
require __DIR__ . '/../vendor/autoload.php';

use App\Container\Logger;
use App\Vega;

const APP_DEBUG = false;

App\Error::register();

$vega = Vega::new();
$http = new Workerman\Worker("http://0.0.0.0:2345");
$http->onMessage = $vega->handler();
$http->count = (int)shell_exec('nproc') * 4;
Logger::instance()->info('Start workerman server');
Workerman\Worker::runAll();
