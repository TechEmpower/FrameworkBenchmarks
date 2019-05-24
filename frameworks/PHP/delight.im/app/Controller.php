<?php

namespace App;

use Delight\Db\PdoDatabase;
use Delight\Foundation\App;

class Controller {

    // Work around described in https://github.com/delight-im/PHP-DB/issues/2 for establishing persistent connection
    public static function getDb() {
        $dsn = "${_ENV['DB_DRIVER']}:host=${_ENV['DB_HOST']};dbname=${_ENV['DB_NAME']};port=${_ENV['DB_PORT']};charset=${_ENV['DB_CHARSET']}";
        $pdo = new \PDO($dsn, $_ENV['DB_USERNAME'], $_ENV['DB_PASSWORD'], [\PDO::ATTR_PERSISTENT => true]);
        $db = PdoDatabase::fromPdo($pdo);
        return $db;
    }

    public static function plaintext(App $app) {
        header('Content-type: text/plain');
        echo 'Hello, World!';
    }

    public static function json(App $app) {
        header('Content-type: application/json');
        echo json_encode(['message' => 'Hello, World!']);
    }

    public static function db(App $app) {
        $rows = Controller::getDb()->select(
            'SELECT * FROM World WHERE id = ?',
            [ mt_rand(1, 10000) ]
        );
        header('Content-type: application/json');
        echo json_encode($rows[0]);
    }

    public static function queries(App $app, $queries = 1)
    {
        $db = Controller::getDb();
        $queries = $app->input()->value($queries, TYPE_INT);
        $queries = max(1, min($queries, 500));
        $rows = [];
        while ($queries--) {
            $row = $db->select(
                'SELECT * FROM World WHERE id = ?',
                [mt_rand(1, 10000)]
            );
            $rows[] = $row[0];
        }
        header('Content-type: application/json');
        echo json_encode($rows);
    }

    public static function updates(App $app, $queries = 1) {
        $db = Controller::getDb();
        $queries = $app->input()->value($queries, TYPE_INT);
        $queries = max(1, min($queries, 500));
        $rows = [];
        while ($queries--) {
            $id = mt_rand(1, 10000);
            $row = $db->select(
                'SELECT randomNumber FROM World WHERE id = ?',
                [ $id ]
            );

            $randomNumber = mt_rand(1, 10000);

            // store result in array to satisfy the spirit of the test case
            $world = ['id' => $id, 'randomNumber' => $row[0]['randomNumber']];
            // modify entity
            $world['randomNumber'] = $randomNumber;
            // store result in array
            $rows[] = $world;
            // update database
            $db->update(
                'World',
                [ 'randomNumber' => $randomNumber],
                [ 'id' => $id ]
            );
        }

        header('Content-type: application/json');
        echo json_encode($rows);
    }

    public static function fortunes(App $app) {
        $fortunes = Controller::getDb()->select(
            'SELECT * FROM Fortune'
        );
        $fortunes = array_column($fortunes, 'message', 'id');
        $fortunes[0] = 'Additional fortune added at request time.';
        asort($fortunes);

        echo $app->view('fortunes.html.twig', ['fortunes' => $fortunes]);
    }
}
