<?php

declare(strict_types=1);

namespace App\Providers;

use Hypervel\Foundation\Support\Providers\RouteServiceProvider as BaseServiceProvider;
use Hypervel\Support\Facades\Route;

class RouteServiceProvider extends BaseServiceProvider
{
    /**
     * The route files for the application.
     */
    protected array $routes = [
    ];

    public function boot(): void
    {
        parent::boot();

        Route::group(
            '/api',
            base_path('routes/api.php'),
            ['middleware' => 'api']
        );

        Route::group(
            '/',
            base_path('routes/web.php'),
            ['middleware' => 'web']
        );
    }
}
