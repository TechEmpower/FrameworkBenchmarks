<?php

namespace App\Model;

use One\Database\Mysql\Model;

class World extends Model
{
    const TABLE = 'world';

    protected $_pri_key = 'id';

    protected $_cache_time = 0;

    public int $id;

    public int $randomNumber;

}