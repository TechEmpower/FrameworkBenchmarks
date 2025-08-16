<?php

declare(strict_types=1);

namespace App\Http\Middleware;

use Hypervel\Foundation\Http\Middleware\ConvertEmptyStringsToNull as Middleware;

class ConvertEmptyStringsToNull extends Middleware
{
    /**
     * The names of the attributes that should not be transformed to null.
     *
     * @var array<int, string>
     */
    protected array $except = [
    ];
}
