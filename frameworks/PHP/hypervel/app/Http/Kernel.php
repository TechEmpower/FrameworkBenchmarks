<?php

declare(strict_types=1);

namespace App\Http;

use Hypervel\Foundation\Http\Kernel as HttpKernel;

class Kernel extends HttpKernel
{
    /**
     * The application's global HTTP middleware stack.
     *
     * These middleware are run during every request to your application.
     *
     * @var array<int, class-string|string>
     */
    protected array $middleware = [
        // \App\Http\Middleware\TrimStrings::class,
        // \Hypervel\Http\Middleware\HandleCors::class,
        // \App\Http\Middleware\ConvertEmptyStringsToNull::class
    ];

    /**
     * The application's route middleware groups.
     *
     * @var array<string, array<int, class-string|string>>
     */
    protected array $middlewareGroups = [
        'web' => [
            // \Hypervel\Router\Middleware\SubstituteBindings::class,
            // \Hypervel\Cookie\Middleware\AddQueuedCookiesToResponse::class,
            // \Hypervel\Session\Middleware\StartSession::class,
            // \Hypervel\View\Middleware\ShareErrorsFromSession::class,
            // \App\Http\Middleware\VerifyCsrfToken::class,
        ],

        'api' => [
            // 'throttle:60,1,api',
            // \Hypervel\Router\Middleware\SubstituteBindings::class,
        ],
    ];

    /**
     * The application's middleware aliases.
     *
     * Aliases may be used instead of class names to conveniently assign middleware to routes and groups.
     *
     * @var array<string, class-string|string>
     */
    protected array $middlewareAliases = [
        // 'auth' => \App\Http\Middleware\Authenticate::class,
        // 'can' => \Hypervel\Auth\Middleware\Authorize::class,
        // 'throttle' => \Hypervel\Router\Middleware\ThrottleRequests::class,
        // 'bindings' => \Hypervel\Router\Middleware\SubstituteBindings::class,
        // 'signed' => \App\Http\Middleware\ValidateSignature::class,
    ];

    /**
     * The priority-sorted list of middleware.
     *
     * Forces non-global middleware to always be in the given order.
     *
     * @var string[]
     */
    protected array $middlewarePriority = [
        // \Hypervel\Router\Middleware\ThrottleRequests::class,
        // \Hypervel\Router\Middleware\SubstituteBindings::class,
        // \Hypervel\Session\Middleware\StartSession::class,
        // \Hypervel\View\Middleware\ShareErrorsFromSession::class,
        // \App\Http\Middleware\VerifyCsrfToken::class,
    ];
}
