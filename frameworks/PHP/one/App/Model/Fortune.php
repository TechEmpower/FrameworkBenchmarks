<?php

namespace App\Model;

use One\Database\Mysql\Model;

class Fortune extends Model
{
    const TABLE = 'fortune';

    protected $_pri_key = 'id';

    protected $_cache_time = 0;
}