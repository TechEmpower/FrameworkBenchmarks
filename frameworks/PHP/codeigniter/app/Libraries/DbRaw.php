<?php

namespace App\Libraries;

use Config\Database;

class DbRaw
{
    public static $db;

    public static function get()
    {
        return self::$db ?? self::$db = Database::connect();
    }
}
