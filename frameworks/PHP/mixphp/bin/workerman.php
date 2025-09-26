<?php
require __DIR__ . '/../vendor/autoload.php';

use App\Container\Logger;
use App\Vega;

App\Error::register();

$cpuCount = function () {
    if (strtolower(PHP_OS) === 'darwin') {
        $count = shell_exec('sysctl -n machdep.cpu.core_count');
    } else {
        $count = shell_exec('nproc');
    }
    $count = (int)$count > 0 ? (int)$count : 4;
    return $count;
};

$vega = Vega::new();
$http = new Workerman\Worker("http://0.0.0.0:2345");
if (\version_compare(\PHP_VERSION, '7.0.0', 'ge') // if php >= 7.0.0
    && \version_compare(php_uname('r'), '3.9', 'ge') // if kernel >=3.9
    && \strtolower(\php_uname('s')) !== 'darwin' // if not Mac OS
) { // if not unix socket
    $http->reusePort = true;
}
$http->transport = 'tcp';
$http->onMessage = $vega->handler();
$http->count = $cpuCount() * 4;
Logger::instance()->info('Start workerman server');
Workerman\Worker::runAll();
