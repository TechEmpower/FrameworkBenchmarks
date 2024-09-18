<?php
declare(strict_types=1);

use Swoole\Database\PDOConfig;
use Swoole\Database\PDOPool;
use Swoole\Database\PDOProxy;
use Swoole\Database\PDOStatementProxy;

class Operation
{
    public const WORLD_SELECT_SQL = 'SELECT id,randomNumber FROM World WHERE id = ?';
    public const FORTUNE_SQL = 'SELECT id, message FROM Fortune';
    public const WORLD_UPDATE_SQL = 'UPDATE World SET randomNumber = ? WHERE id = ?';

    public static function db(PDOStatement|PDOStatementProxy $db): string
    {
        $db->execute([mt_rand(1, 10000)]);
        return json_encode($db->fetch(PDO::FETCH_ASSOC), JSON_NUMERIC_CHECK);
    }

    public static function fortunes(PDOStatement|PDOStatementProxy $fortune): string
    {
        $fortune->execute();
        $results    = $fortune->fetchAll(PDO::FETCH_KEY_PAIR);
        $results[0] = 'Additional fortune added at request time.';
        asort($results);

        $html = '';
        foreach ($results as $id => $message) {
            $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
            $html    .= "<tr><td>$id</td><td>$message</td></tr>";
        }

        return "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>";
    }

    public static function query(PDOStatement|PDOStatementProxy $query, int $queries): string
    {
        $results = [];
        while ($queries--) {
            $query->execute([mt_rand(1, 10000)]);
            $results[] = $query->fetch(PDO::FETCH_ASSOC);
        }

        return json_encode($results, JSON_NUMERIC_CHECK);
    }

    public static function updates(PDOStatement|PDOStatementProxy $random, PDOStatement|PDOStatementProxy $update, int $queries, string $driver): string
    {
        $results = $keys = $values = [];
        while ($queries--) {
            $random->execute([mt_rand(1, 10000)]);
            $item                 = $random->fetch(PDO::FETCH_ASSOC);
            $item['randomNumber'] = mt_rand(1, 10000);
            $results[]            = $item;

            if ($driver == 'pgsql') {
                $values[] = $keys[] = $item['id'];
                $values[] = $item['randomNumber'];
            } else {
                $update->execute([$item['randomNumber'], $item['id']]);
            }
        }

        if ($driver == 'pgsql') {
            $update->execute([...$values, ...$keys]);
        }
        return json_encode($results, JSON_NUMERIC_CHECK);
    }
}

class Connection
{
    private static PDO $pdo;
    private static string $driver;
    private static array $updates = [];
    private static PDOStatement $db;
    private static PDOStatement $fortune;
    private static PDOStatement $random;
    private static PDOStatement $query;

    public static function init(string $driver): void
    {
        $pdo = new PDO(
            "$driver:host=tfb-database;dbname=hello_world",
            "benchmarkdbuser",
            "benchmarkdbpass",
            [
                PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
                PDO::ATTR_ERRMODE            => PDO::ERRMODE_EXCEPTION,
                PDO::ATTR_EMULATE_PREPARES   => false
            ]
        );

        self::$db      = self::$random = self::$query = $pdo->prepare(Operation::WORLD_SELECT_SQL);
        self::$fortune = $pdo->prepare(Operation::FORTUNE_SQL);

        self::$pdo    = $pdo;
        self::$driver = $driver;
    }

    public static function db(): string
    {
        return Operation::db(self::$db);
    }

    public static function fortunes(): string
    {
        return Operation::fortunes(self::$fortune);
    }

    public static function query(int $queries): string
    {
        return Operation::query(self::$query, $queries);
    }

    public static function updates(int $queries): string
    {
        if (!isset(self::$updates[$queries])) {
            self::$updates[$queries] = self::$driver == 'pgsql'
                ? self::$pdo->prepare('UPDATE World SET randomNumber = CASE id'.\str_repeat(' WHEN ?::INTEGER THEN ?::INTEGER ', $queries).'END WHERE id IN ('.\str_repeat('?::INTEGER,', $queries - 1).'?::INTEGER)')
                : self::$pdo->prepare(Operation::WORLD_UPDATE_SQL);
        }

        return Operation::updates(self::$random, self::$updates[$queries], $queries, self::$driver);
    }
}

class Connections
{
    private static PDOPool $pool;
    private static string $driver;
    private static array $dbs = [];
    private static array $fortunes = [];
    private static array $updates = [];

    public static function init(string $driver): void
    {
        $config = (new PDOConfig())
            ->withDriver($driver)
            ->withHost('tfb-database')
            ->withPort($driver == 'mysql' ? 3306 : 5432)
            ->withDbName('hello_world')
            ->withUsername('benchmarkdbuser')
            ->withPassword('benchmarkdbpass');

        self::$pool   = new PDOPool($config, intval(1400 / swoole_cpu_num()));
        self::$driver = $driver;
    }

    public static function db(): string
    {
        $pdo    = self::get();
        $result = Operation::db(self::getStatement($pdo, 'select'));
        self::put($pdo);
        return $result;
    }

    public static function fortunes(): string
    {
        $pdo    = self::get();
        $result = Operation::fortunes(self::getStatement($pdo, 'fortunes'));
        self::put($pdo);

        return $result;
    }

    public static function query(int $queries): string
    {
        $pdo    = self::get();
        $result = Operation::query(self::getStatement($pdo, 'select'), $queries);
        self::put($pdo);

        return $result;
    }

    public static function updates(int $queries): string
    {
        $pdo    = self::get();
        $result = Operation::updates(self::getStatement($pdo, 'select'), self::getStatement($pdo, 'update', $queries), $queries, self::$driver);
        self::put($pdo);

        return $result;
    }

    private static function get(): PDO|PDOProxy
    {
        return self::$pool->get();
    }

    private static function put(PDO|PDOProxy $db): void
    {
        self::$pool->put($db);
    }

    private static function getStatement(PDO|PDOProxy $pdo, string $type, int $queries = 0): PDOStatement|PDOStatementProxy
    {
        $hash = spl_object_id($pdo);

        if ('select' == $type) {
            if (!isset(self::$dbs[$hash])) {
                self::$dbs[$hash] = $pdo->prepare(Operation::WORLD_SELECT_SQL);
            }

            return self::$dbs[$hash];
        } elseif ('fortunes' == $type) {
            if (!isset(self::$fortunes[$hash])) {
                self::$fortunes[$hash] = $pdo->prepare(Operation::FORTUNE_SQL);
            }

            return self::$fortunes[$hash];
        } else {
            if (!isset(self::$updates[$hash][$queries])) {
                self::$updates[$hash][$queries] = self::$driver == 'pgsql'
                    ? $pdo->prepare('UPDATE World SET randomNumber = CASE id'.\str_repeat(' WHEN ?::INTEGER THEN ?::INTEGER ', $queries).'END WHERE id IN ('.\str_repeat('?::INTEGER,', $queries - 1).'?::INTEGER)')
                    : $pdo->prepare(Operation::WORLD_UPDATE_SQL);
            }

            return self::$updates[$hash][$queries];
        }
    }
}
