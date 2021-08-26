<?php
require __DIR__ . '/../vendor/autoload.php';

use App\Container\Logger;
use App\Vega;

App\Error::register();

/**
 * 多进程默认开启了协程
 * 关闭协程只需关闭 `enable_coroutine` 配置并注释数据库的 `::enableCoroutine()` 即可退化为多进程同步模式
 */

$vega = Vega::new();
$http = new Swoole\Http\Server('0.0.0.0', 9501, SWOOLE_BASE, SWOOLE_SOCK_TCP);
$http->on('Request', $vega->handler());
$http->on('WorkerStart', function ($server, $workerId) {
    // swoole 协程不支持 set_exception_handler 需要手动捕获异常
    try {
        // App\Container\DB::enableCoroutine();
    } catch (\Throwable $ex) {
        App\Error::handle($ex);
    }
});
$http->set([
    'enable_coroutine' => false,
    'worker_num' => swoole_cpu_num() * 4,
    'log_file' => '/dev/null',
    'log_level' => SWOOLE_LOG_ERROR,
]);
Logger::instance()->info('Start swoole server');
$http->start();
