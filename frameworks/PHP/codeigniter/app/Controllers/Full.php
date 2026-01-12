<?php

namespace App\Controllers;

use CodeIgniter\Controller;
use function asort;
use function is_numeric;
use function max;
use function min;
use function mt_rand;

class Full extends Controller
{
    public function db()
    {
        return $this->response->setJSON(model('World')->find(mt_rand(1, 10000)));
    }

    public function queries($queries = 1)
    {
        $queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;
        $world = [];

        while ($queries--) {
            $world[] = model('World')->find(mt_rand(1, 10000));
        }
        
        return $this->response->setJSON($world);
    }

    public function update($queries = 1)
    {
        $queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;
        $world = [];

        while ($queries--) {
            $row = model('World')->find(mt_rand(1, 10000));
            $row['randomNumber'] = mt_rand(1, 10000);
            $world[] = $row;
        }

        model('World')->builder->updateBatch($world, 'id');

        return $this->response->setJSON($world);
    }

    public function fortunes()
    {
        $fortunes = array_column(model('Fortune')->findAll(), 'message', 'id');
        $fortunes[0] = 'Additional fortune added at request time.';
        asort($fortunes);

        return service('renderer')->setVar('fortunes', $fortunes)->render('fortunes');
    }
}
