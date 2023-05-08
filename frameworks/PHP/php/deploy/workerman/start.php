<?php

use Adapterman\Adapterman;
use Workerman\Worker;
use Workerman\Lib\Timer;

require_once __DIR__ . '/vendor/autoload.php';

Adapterman::init();
// WebServer
$web = new Worker("http://0.0.0.0:8080");
$web->count = (int) shell_exec('nproc') * 4;
$web->name = 'workerman';

define('WEBROOT', '/php/');

$web->onWorkerStart = static function () {
    Header::init();
};

$web->onMessage = static function ($connection) {
    $path = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
    /* if ($path === '/') {
        $connection->send(exec_php_file(WEBROOT.'/index.php', $request));
        return;
    } */

    $file = realpath(WEBROOT . $path);
    if (false === $file || !str_ends_with($file, '.php')) {
        http_response_code(404);
        $connection->send('<h3>404 Not Found</h3>');
        return;
    }
    // Security check!
    if (!str_starts_with($file, WEBROOT)) {
        http_response_code(400);
        $connection->send('<h3>400 Bad Request</h3>');
        return;
    }

    header(Header::$date); // To pass the bench
    $connection->send(exec_php_file($file));
};

function exec_php_file($file)
{
    ob_start();
    // Try to include php file.
    try {
        include $file;
    } catch (Throwable $t) {
        echo $t;
    }
    return ob_get_clean();
}

class Header
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
        self::$date = self::NAME . gmdate('D, d M Y H:i:s').' GMT';
        Timer::add(1, static function() {
            self::$date = self::NAME . gmdate('D, d M Y H:i:s').' GMT';
        });
    }
}

Worker::runAll();
