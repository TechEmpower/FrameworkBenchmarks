<?php

namespace App\Models;

use CodeIgniter\Model;

class World extends Model
{
    protected $table         = 'World';
    protected $primaryKey    = 'id';
    protected array $casts   = [
        'id'           => 'int',
        'randomNumber' => 'int',
    ];
    protected $allowedFields = ['randomNumber'];
}
