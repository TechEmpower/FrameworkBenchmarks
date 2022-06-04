<?php

class KuRaw
{
    private static PDO $instance;
    public static PDOStatement $db;
    public static PDOStatement $fortune;
    public static PDOStatement $random;
    /**
     * @var []PDOStatement
     */
    private static $update;

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
        self::$instance  = $pdo;
    }

    /**
     * Postgres bulk update
     *
     * @param array $worlds
     * @return void
     */
    public static function update(array $worlds)
    {
        $rows = count($worlds);

        if (!isset(self::$update[$rows])) {
            $sql = 'UPDATE world SET randomNumber = CASE id'
                . str_repeat(' WHEN ?::INTEGER THEN ?::INTEGER ', $rows)
                . 'END WHERE id IN ('
                . str_repeat('?::INTEGER,', $rows - 1) . '?::INTEGER)';

            self::$update[$rows] = self::$instance->prepare($sql);
        }

        $val = [];
        $keys = [];
        foreach ($worlds as $world) {
            $val[] = $keys[] = $world['id'];
            $val[] = $world['randomNumber'];
        }

        self::$update[$rows]->execute([...$val, ...$keys]);
    }

    /**
     * Alternative bulk update in Postgres
     *
     * @param array $worlds
     * @return void
     */
    public static function update2(array $worlds)
    {
        $rows = count($worlds);

        if (!isset(self::$update[$rows])) {
            $sql = 'UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES '
                . implode(', ', array_fill(0, $rows, '(?::INTEGER, ?::INTEGER)')) .
                ' ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id';

            self::$update[$rows] = self::$instance->prepare($sql);
        }

        $val = [];
        foreach ($worlds as $world) {
            $val[] = $world['id'];
            $val[] = $world['randomNumber'];
            //$update->bindParam(++$i, $world['id'], PDO::PARAM_INT);
        }

        self::$update[$rows]->execute($val);
    }
}
