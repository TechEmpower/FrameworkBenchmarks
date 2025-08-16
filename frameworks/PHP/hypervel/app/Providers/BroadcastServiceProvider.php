<?php

declare(strict_types=1);

namespace App\Providers;

use Hypervel\Support\Facades\Broadcast;
use Hypervel\Support\ServiceProvider;

class BroadcastServiceProvider extends ServiceProvider
{
    /**
     * Bootstrap any application services.
     */
    public function boot(): void
    {
        Broadcast::routes();

        require base_path('routes/channels.php');
    }
}
