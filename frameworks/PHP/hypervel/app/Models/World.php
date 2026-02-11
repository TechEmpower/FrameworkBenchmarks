<?php

declare(strict_types=1);

namespace App\Models;

use App\Models\Model;

class World extends Model
{
	protected ?string $table = 'World';

	public bool $timestamps = false;
}
