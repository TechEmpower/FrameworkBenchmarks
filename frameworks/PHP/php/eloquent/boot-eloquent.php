<?php

require_once __DIR__ . '/../vendor/autoload.php';
require_once __DIR__ . '/init-capsule.php';

use Illuminate\Database\Capsule\Manager as Capsule;

class World extends \Illuminate\Database\Eloquent\Model {
    protected $table = 'World';
    public $timestamps = false;
}

$capsule->bootEloquent();

