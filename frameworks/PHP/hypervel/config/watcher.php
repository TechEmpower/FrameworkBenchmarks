<?php

declare(strict_types=1);

use Hyperf\Watcher\Driver\ScanFileDriver;

return [
    'driver' => ScanFileDriver::class,
    'bin' => PHP_BINARY,
    'command' => 'artisan serve',
    'watch' => [
        'dir' => ['app', 'config', 'routes', 'resources'],
        'file' => ['.env'],
        'scan_interval' => 2000,
    ],
    'ext' => ['.php', '.env'],
];
