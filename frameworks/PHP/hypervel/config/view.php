<?php

declare(strict_types=1);

use Hyperf\View\Mode;
use Hyperf\ViewEngine\HyperfViewEngine;

return [
    'engine' => HyperfViewEngine::class,
    'mode' => Mode::SYNC,
    'config' => [
        'view_path' => base_path('resources/views'),
        'cache_path' => storage_path('framework/views'),
    ],
    'event' => [
        'enable' => false,
    ],
    'components' => [
    ],
];
