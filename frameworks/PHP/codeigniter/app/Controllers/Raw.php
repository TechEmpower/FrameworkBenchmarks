<?php

namespace App\Controllers;

use CodeIgniter\Controller;
use function asort;
use function is_numeric;
use function max;
use function min;
use function mt_rand;

class Raw extends Controller
{
    public function db()
    {
        return $this->response->setJSON(
            db_connect()->query('SELECT id, randomNumber FROM World WHERE id = ?', mt_rand(1, 10000))->getRowArray()
        );
    }

    public function queries($queries = 1)
    {
        $db = db_connect();

        $select = $db->prepare(
            fn () => $db->query('SELECT id, randomNumber FROM World WHERE id = ?')
        );

        $queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;
        $world = [];

        while ($queries--) {
            $world[] = $select->execute(mt_rand(1, 10000))->getRowArray();
        }

        return $this->response->setJSON($world);
    }

    public function update($queries = 1)
    {
        $db = db_connect();

        $select = $db->prepare(
            fn () => $db->query('SELECT id, randomNumber FROM World WHERE id = ?')
        );

        $update = $db->prepare(
            fn () => $db->query('UPDATE World SET randomNumber = ? WHERE id = ?')
        );

        $queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;
        $world = [];

        while ($queries--) {
            $row = $select->execute(mt_rand(1, 10000))->getRowArray();
            $row['randomNumber'] = mt_rand(1, 10000);
            $update->execute($row);
            $world[] = $row;
        }

        return $this->response->setJSON($world);
    }

    public function fortunes()
    {
        $fortunes = array_column(
            db_connect()->query('SELECT id, message FROM Fortune')->getResultArray(), 'message', 'id'
        );
        $fortunes[0] = 'Additional fortune added at request time.';
        asort($fortunes);

        return service('renderer')->setVar('fortunes', $fortunes)->render('fortunes');
    }
}
