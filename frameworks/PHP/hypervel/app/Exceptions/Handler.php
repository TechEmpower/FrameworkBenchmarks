<?php

declare(strict_types=1);

namespace App\Exceptions;

use Hypervel\Foundation\Exceptions\Handler as ExceptionHandler;
use Hypervel\Http\Request;
use Throwable;

class Handler extends ExceptionHandler
{
    /**
     * The list of the inputs that are never flashed to the session on validation exceptions.
     *
     * @var array<int, string>
     */
    protected array $dontFlash = [
        'current_password',
        'password',
        'password_confirmation',
    ];

    /**
     * Register the exception handling callbacks for the application.
     */
    public function register(): void
    {
        // return json when path start with `api`
        $this->shouldRenderJsonWhen(function (Request $request, Throwable $e) {
            return str_starts_with($path = $request->path(), 'api')
                && (strlen($path) === 3 || $path[3] === '/');
        });

        $this->reportable(function (Throwable $e) {
        });
    }
}
