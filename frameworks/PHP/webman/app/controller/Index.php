<?php
namespace app\controller;

use support\Request;
use support\bootstrap\Date;
use support\bootstrap\db\Raw as Db;
use support\Response;
use function json_encode;
use function max;
use function min;
use function mt_rand;

class Index
{
    public function plaintext()
    {
        return new Response(200, [
            'Content-Type' => 'text/plain',
            'Date'         => Date::$date
        ], 'Hello, World!');
    }


    public function json()
    {
        return new Response(200, [
            'Content-Type' => 'application/json',
            'Date'         => Date::$date
        ], json_encode(['message' => 'Hello, World!']));
    }

    public function db()
    {
        $statement = Db::$random;
        $statement->execute([mt_rand(1, 10000)]);

        return new Response(200, [
            'Content-Type' => 'application/json',
            'Date'         => Date::$date
        ], json_encode($statement->fetch()));
    }

    public function fortunes()
    {
        $fortune = Db::$fortune;

        $fortune->execute();

        $arr    = $fortune->fetchAll(\PDO::FETCH_KEY_PAIR);
        $arr[0] = 'Additional fortune added at request time.';
        \asort($arr);

        $html = '';
        foreach ($arr as $id => $message) {
            $message = \htmlspecialchars($message, \ENT_QUOTES, 'UTF-8');
            $html .= "<tr><td>$id</td><td>$message</td></tr>";
        }

        return new Response(200, [
            'Date'         => Date::$date
        ], "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>"
        );
    }

    public function queries(Request $request, $q = 1)
    {
        $statement = Db::$random;

        $query_count = min(max((int) $q, 1), 500);

        $arr = [];
        while ($query_count--) {
            $statement->execute([mt_rand(1, 10000)]);
            $arr[] = $statement->fetch();
        }

        return new Response(200, [
            'Content-Type' => 'application/json',
            'Date'         => Date::$date
        ], json_encode($arr));
    }

    public function updates(Request $request, $q = 1)
    {
        static $updates = [];

        $random = Db::$random;
        $pdo = Db::$pdo;
        $count = min(max((int) $q, 1), 500);

        $worlds = $keys = $values = [];
        for ($i = 0; $i < $count; ++ $i) {
            $values[] = $keys[] = $id = mt_rand(1, 10000);
            $random->execute([$id]);
            $row = $random->fetch();
            $values[] = $row['randomNumber'] = mt_rand(1, 10000);
            $worlds[] = $row;
        }
        if (!isset($updates[$count])) {
            $sql = 'UPDATE World SET randomNumber = CASE id' . str_repeat(' WHEN ?::INTEGER THEN ?::INTEGER ', $count) . 'END WHERE id IN (' . str_repeat('?::INTEGER,', $count - 1) . '?::INTEGER)';
            $updates[$count] = $pdo->prepare($sql);
        }
        $updates[$count]->execute([...$values, ...$keys]);

        return new Response(200, [
            'Content-Type' => 'application/json',
            'Date'         => Date::$date
        ], json_encode($worlds));

    }


}
