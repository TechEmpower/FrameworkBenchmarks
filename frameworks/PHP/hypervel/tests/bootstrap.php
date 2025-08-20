<?php

declare(strict_types=1);

use Hypervel\Foundation\Testing\TestScanHandler;

ini_set('display_errors', 'on');
ini_set('display_startup_errors', 'on');

error_reporting(E_ALL);

! defined('SWOOLE_HOOK_FLAGS') && define('SWOOLE_HOOK_FLAGS', SWOOLE_HOOK_ALL);

$dir = __DIR__;
$lastDir = '';
if (! defined('BASE_PATH')) {
    while (! file_exists($dir . '/composer.json') && $dir !== dirname($dir)) {
        if ($lastDir === $dir) {
            break;
        }
        $lastDir = $dir;
        $dir = dirname($dir);
    }
}

if (! file_exists($dir . '/composer.json')) {
    throw new RuntimeException('Unable to find base path (directory with composer.json)');
}

define('BASE_PATH', $dir);

require BASE_PATH . '/vendor/autoload.php';

Hypervel\Foundation\ClassLoader::init(null, null, new TestScanHandler());

$app = require BASE_PATH . '/bootstrap/app.php';

$app->get(Hyperf\Contract\ApplicationInterface::class);
