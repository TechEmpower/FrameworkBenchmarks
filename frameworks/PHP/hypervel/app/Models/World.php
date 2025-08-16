<?php

declare(strict_types=1);

namespace App\Models;

use App\Models\Model;

class World extends Model
{
    /**
     * The attributes that are mass assignable.
     */
    protected array $fillable = [];

    /**
     * The attributes that should be cast to native types.
     */
    protected array $casts = [];
}
