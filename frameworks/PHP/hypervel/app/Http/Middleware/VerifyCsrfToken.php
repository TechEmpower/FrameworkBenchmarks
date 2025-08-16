<?php

declare(strict_types=1);

namespace App\Http\Middleware;

use Hypervel\Foundation\Http\Middleware\VerifyCsrfToken as Middleware;

class VerifyCsrfToken extends Middleware
{
    /**
     * The URIs that should be excluded from CSRF verification.
     *
     * @var array<int, string>
     */
    protected array $except = [
    ];
}
