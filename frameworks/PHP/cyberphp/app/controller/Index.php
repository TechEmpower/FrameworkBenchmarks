<?php
namespace app\controller;

use Cyber\Response;
class Index {
    public function hello() {
        return 'Hello, CyberPHP';
    }

    public function json()
    {
        return Response::json(['message' => 'Hello, World!']);
    }

    public function plaintext()
    {
        return Response::text('Hello, World!');
    }

    public function db()
    {
        $prepare = app()->db->prepare('SELECT id,randomNumber FROM World WHERE id=?');
        $prepare->execute([mt_rand(1, 10000)]);
        return Response::json($prepare->fetch());
    }
    public function fortunes()
    {
        $fortune = app()->db->prepare('SELECT id,message FROM Fortune');
        $fortune->execute();
        $arr    = $fortune->fetchAll(\PDO::FETCH_KEY_PAIR);
        $arr[0] = 'Additional fortune added at request time.';
        \asort($arr);
        $html = '';
        foreach ($arr as $id => $message) {
            $message = \htmlspecialchars($message, \ENT_QUOTES, 'UTF-8');
            $html .= "<tr><td>$id</td><td>$message</td></tr>";
        }
        return Response::html("<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>");
    }

    public function queries($q = 1)
    {
        $statement = app()->db->prepare('SELECT id,randomNumber FROM World WHERE id=?');
        $query_count = min(max((int) $q, 1), 500);
        $arr = [];
        while ($query_count--) {
            $statement->execute([mt_rand(1, 10000)]);
            $arr[] = $statement->fetch();
        }
        return Response::json($arr);
    }

    public function updates($q = 1)
    {
        static $updates = [];

        $random = app()->db->prepare('SELECT id,randomNumber FROM World WHERE id=?');
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
            $updates[$count] = app()->db->prepare($sql);
        }
        $updates[$count]->execute([...$values, ...$keys]);

        return Response::json($worlds);
    }
}