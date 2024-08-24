<?php

class KuRaw
{
    private static PDO $instance;
    public static PDOStatement $db;
    public static PDOStatement $fortune;
    public static PDOStatement $random;
    public static PDOStatement $update;

    public static function init()
    {
        $pdo = new PDO(
            'pgsql:host=tfb-database;dbname=hello_world',
            'benchmarkdbuser',
            'benchmarkdbpass',
            [
                PDO::ATTR_DEFAULT_FETCH_MODE  => PDO::FETCH_ASSOC,
                PDO::ATTR_ERRMODE             => PDO::ERRMODE_EXCEPTION,
                PDO::ATTR_EMULATE_PREPARES    => false
            ]
        );

        self::$fortune   = $pdo->prepare('SELECT id,message FROM Fortune');
        self::$random    = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id = ?');
        self::$update    = $pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');
        self::$instance  = $pdo;
    }
}
