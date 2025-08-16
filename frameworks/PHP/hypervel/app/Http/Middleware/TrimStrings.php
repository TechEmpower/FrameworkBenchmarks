<?php

declare(strict_types=1);

namespace App\Http\Middleware;

use Hypervel\Foundation\Http\Middleware\TrimStrings as Middleware;

class TrimStrings extends Middleware
{
    /**
     * The names of the attributes that should not be trimmed.
     *
     * @var array<int, string>
     */
    protected array $except = [
    ];
}
