<?php

declare(strict_types=1);

namespace App\Providers;

use Hypervel\Foundation\Support\Providers\EventServiceProvider as BaseServiceProvider;

class EventServiceProvider extends BaseServiceProvider
{
    /**
     * The event listener mappings for the application.
     */
    protected array $listen = [
        \App\Events\DemoEvent::class => [
            \App\Listeners\DemoListener::class,
        ],
    ];
}
