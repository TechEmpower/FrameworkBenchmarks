<?php

declare(strict_types=1);

namespace App\Models;

use App\Models\Model;

class Fortune extends Model
{
	protected ?string $table = 'Fortune';

	public bool $timestamps = false;
}
