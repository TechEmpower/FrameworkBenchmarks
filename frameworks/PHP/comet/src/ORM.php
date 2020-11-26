<?php
declare(strict_types=1);

namespace App;

class ORM
{
    public static $pdo;
    public static $statement;
    public static $fortune;
    public static $random;
    public static $update;

    public static function init()
    {
        self::$pdo = new \PDO(
            'pgsql:host=tfb-database;dbname=hello_world',
//	    	'pgsql:host=192.168.99.1;dbname=hello_world',
            'benchmarkdbuser',
            'benchmarkdbpass',
            [
//              \PDO::ATTR_CASE => \PDO::CASE_NATURAL,
                \PDO::ATTR_DEFAULT_FETCH_MODE => \PDO::FETCH_ASSOC,
                \PDO::ATTR_EMULATE_PREPARES => false
            ]
        );

        self::$statement = self::$pdo->prepare('SELECT id, randomNumber FROM World WHERE id=?');
        self::$fortune   = self::$pdo->prepare('SELECT id, message FROM Fortune');
        self::$random    = self::$pdo->prepare('SELECT randomNumber, id FROM World WHERE id=?');
        self::$update    = self::$pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');
    }
}

