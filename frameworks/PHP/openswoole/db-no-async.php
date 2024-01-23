<?php
class Db
{
    public static PDOStatement $db;
    public static PDOStatement $fortune;
    public static PDOStatement $random;
    public static PDOStatement $update;

    public static function init()
    {
        $pdo = new PDO(
            "mysql:host=tfb-database;dbname=hello_world",
            "benchmarkdbuser",
            "benchmarkdbpass",
            [
                PDO::ATTR_DEFAULT_FETCH_MODE  => PDO::FETCH_ASSOC,
                PDO::ATTR_ERRMODE             => PDO::ERRMODE_EXCEPTION,
                PDO::ATTR_EMULATE_PREPARES    => false
            ]
        );

        self::$db        = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id = ?');
        self::$fortune   = $pdo->prepare('SELECT id,message FROM Fortune');
        self::$random    = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id = ?');
        self::$update    = $pdo->prepare('UPDATE World SET randomNumber = ? WHERE id = ?');
    }
}

/**
 * The DB test
 *
 * @return string
 */
function db(): string
{
    Db::$db->execute([mt_rand(1, 10000)]);
    return json_encode(Db::$db->fetch(), JSON_NUMERIC_CHECK);
}

/**
 * The Queries test
 *
 * @param int $queries
 *
 * @return string
 */
function query(int $queries = 1): string
{
    // Read number of queries to run from URL parameter
    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    // Create an array with the response string.
    $arr = [];

    // For each query, store the result set values in the response array
    while ($query_count--) {
        DB::$db->execute([mt_rand(1, 10000)]);
        $arr[] = Db::$db->fetch();
    }

    return json_encode($arr, JSON_NUMERIC_CHECK);
}

/**
 * The Fortunes test
 *
 * @return string
 */
function fortunes(): string
{
    $fortune = [];
    Db::$fortune->execute();
    $fortune = Db::$fortune->fetchAll(PDO::FETCH_KEY_PAIR);

    $fortune[0] = 'Additional fortune added at request time.';
    asort($fortune);

    $html = '';
    foreach ($fortune as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    return "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>";
}

/**
 * The Updates test
 *
 * @param int $queries
 *
 * @return string
 */
function updates(int $queries): string
{
    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    while ($query_count--) {
        $id = mt_rand(1, 10000);
        Db::$random->execute([$id]);

        $world = ["id" => $id, "randomNumber" => Db::$random->fetchColumn()];
        $world['randomNumber'] = mt_rand(1, 10000);
        Db::$update->execute([$world['randomNumber'], $world['id']]);

        $arr[] = $world;
    }

    return json_encode($arr, JSON_NUMERIC_CHECK);
}
