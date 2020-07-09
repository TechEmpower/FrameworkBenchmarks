<?php
namespace app\controller;

use support\Request;
use support\bootstrap\Date;
use support\bootstrap\db\Raw as Db;
use support\Response;
use PDO;

class Index
{
    public function plaintext($request)
    {
        return new Response(200, [
            'Content-Type' => 'text/plain',
            'Date'         => Date::$date
        ], 'Hello, World!');
    }


    public function json($request)
    {
        return new Response(200, [
            'Content-Type' => 'application/json',
            'Date'         => Date::$date
        ], json_encode(['message' => 'Hello, World!']));
    }

    public function db($request)
    {
        $statement = Db::$statement;
        $statement->execute([\mt_rand(1, 10000)]);

        return new Response(200, [
            'Content-Type' => 'application/json',
            'Date'         => Date::$date
        ], json_encode($statement->fetch()));
    }

    public function fortunes($request)
    {
        $fortune = Db::$fortune;

        $fortune->execute();

        $arr    = $fortune->fetchAll(PDO::FETCH_KEY_PAIR);
        $arr[0] = 'Additional fortune added at request time.';
        \asort($arr);

        $html = '';
        foreach ($arr as $id => $message) {
            $message = \htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
            $html .= "<tr><td>$id</td><td>$message</td></tr>";
        }

        return new Response(200, [
            'Date'         => Date::$date
        ], "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>"
        );
    }

    public function queries($request, $q = 1)
    {
        $statement = Db::$statement;

        $query_count = 1;
        if ($q > 1) {
            $query_count = \min($q, 500);
        }

        $arr = [];
        while ($query_count--) {
            $statement->execute([\mt_rand(1, 10000)]);
            $arr[] = $statement->fetch();
        }

        return new Response(200, [
            'Content-Type' => 'application/json',
            'Date'         => Date::$date
        ], json_encode($arr));
    }

    public function updates($request, $q = 1)
    {
        $random = Db::$random;
        $update = Db::$update;

        $query_count = 1;
        if ($q > 1) {
            $query_count = \min($q, 500);
        }

        $arr = [];

        while ($query_count--) {
            $id = \mt_rand(1, 10000);
            $random->execute([$id]);

            //$random->fetchColumn(); //
            //$world = ['id' => $id, 'randomNumber' => \mt_rand(1, 10000)]; //

            $world = ['id' => $id, 'randomNumber' => $random->fetchColumn()];
            $update->execute(
                [$world['randomNumber'] = mt_rand(1, 10000), $id]
            );

            $arr[] = $world;
        }

        /*$pdo = Db::$pdo;
        $pdo->beginTransaction();
        foreach($arr as $world) {
             $update->execute([$world['randomNumber'], $world['id']]);
        }
        $pdo->commit();*/

        return new Response(200, [
            'Content-Type' => 'application/json',
            'Date'         => Date::$date
        ], \json_encode($arr));
    }


}
