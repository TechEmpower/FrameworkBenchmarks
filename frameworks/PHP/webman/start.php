<?php
require_once __DIR__ . '/vendor/autoload.php';

use Workerman\Worker;
use Workerman\Protocols\Http;
use Workerman\Connection\TcpConnection;
use Webman\App;
use Webman\Config;
use Webman\Route;
use Webman\Middleware;
use support\Request;
use support\bootstrap\Log;
use support\bootstrap\Container;

Config::load(config_path(), ['route', 'container']);
$config = config('server');

if ($timezone = config('app.default_timezone')) {
    date_default_timezone_set($timezone);
}

Worker::$onMasterReload = function (){
    if ($status = opcache_get_status()) {
        foreach (array_keys($status['scripts']) as $file) {
            opcache_invalidate($file, true);
        }
    }
};

Worker::$pidFile                      = $config['pid_file'];
Worker::$stdoutFile                   = $config['stdout_file'];
TcpConnection::$defaultMaxPackageSize = $config['max_package_size'] ?? 10*1024*1024;

$worker = new Worker($config['listen'], $config['context']);
$property_map = [
    'name',
    'count',
    'user',
    'group',
    'reusePort',
    'transport',
];
foreach ($property_map as $property) {
    if (isset($config[$property])) {
        $worker->$property = $config[$property];
    }
}

$worker->onWorkerStart = function ($worker) {
    Config::reload(config_path(), ['route', 'container']);
    foreach (config('bootstrap', []) as $class_name) {
        /** @var \Webman\Bootstrap $class_name */
        $class_name::start($worker);
    }
    $app = new App($worker, Container::instance(), Log::channel('default'), app_path(), public_path());
    Route::load(config_path() . '/route.php');
    Middleware::load(config('middleware', []));
    Middleware::load(['__static__' => config('static.middleware', [])]);
    Http::requestClass(Request::class);

    $worker->onMessage = [$app, 'onMessage'];
};


foreach (config('process', []) as $process_name => $config) {
    $worker = new Worker($config['listen'] ?? null, $config['context'] ?? []);
    $property_map = [
        'count',
        'user',
        'group',
        'reloadable',
        'reusePort',
        'transport',
        'protocol',
    ];
    $worker->name = $process_name;
    foreach ($property_map as $property) {
        if (isset($config[$property])) {
            $worker->$property = $config[$property];
        }
    }

    $worker->onWorkerStart = function ($worker) use ($config) {
        Config::reload(config_path(), ['route']);

        $bootstrap = $config['bootstrap'] ?? config('bootstrap', []);
        if (!in_array(support\bootstrap\Log::class, $bootstrap)) {
            $bootstrap[] = support\bootstrap\Log::class;
        }
        foreach ($bootstrap as $class_name) {
            /** @var \Webman\Bootstrap $class_name */
            $class_name::start($worker);
        }

        foreach ($config['services'] ?? [] as $server) {
            if (!class_exists($server['handler'])) {
                echo "process error: class {$config['handler']} not exists\r\n";
                continue;
            }
            $listen = new Worker($server['listen'] ?? null, $server['context'] ?? []);
            if (isset($server['listen'])) {
                echo "listen: {$server['listen']}\n";
            }
            $class = Container::make($server['handler'], $server['constructor'] ?? []);
            worker_bind($listen, $class);
            $listen->listen();
        }

        if (isset($config['handler'])) {
            if (!class_exists($config['handler'])) {
                echo "process error: class {$config['handler']} not exists\r\n";
                return;
            }

            $class = Container::make($config['handler'], $config['constructor'] ?? []);
            worker_bind($worker, $class);
        }

    };
}


Worker::runAll();