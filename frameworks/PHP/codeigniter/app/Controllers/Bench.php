<?php

namespace App\Controllers;

use CodeIgniter\Controller;
use CodeIgniter\Database\BaseConnection;
use Config\Database;

class Bench extends Controller
{
    protected static BaseConnection $database;

    protected function database(): BaseConnection
    {
        return self::$database ??= Database::connect();
    }

    public function plaintext()
    {
        return $this->response->setContentType('text/plain')->setBody('Hello, World!');
    }

    public function json()
    {
        return $this->response->setJSON(['message' => 'Hello, World!']);
    }

    public function db()
    {
        $worlds = $this->database()
            ->query('SELECT * FROM World WHERE id = ?', array(mt_rand(1, 10000)))
            ->getRow();

        return $this->response->setJSON($worlds);
    }

    public function queries($queries = 1)
    {
        $worlds = [];
        $queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $this->database()
                ->query('SELECT * FROM World WHERE id = ?', array(mt_rand(1, 10000)))
                ->getRow();
        }

        return $this->response->setJSON($worlds);
    }

    public function update($queries = 1)
    {
        $worlds = [];
        $queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;

        for ($i = 0; $i < $queries; ++$i) {
            $id = mt_rand(1, 10000);
            $world = $this->database()
                ->query('SELECT * FROM World WHERE id = ?', [$id])
                ->getRow();
            
            $world->randomNumber = mt_rand(1, 10000);
            $this->database()
                ->query('UPDATE World SET randomNumber=? WHERE id=?', [$world->randomNumber, $id]);
            $worlds[] = $world;
        }

        return $this->response->setJSON($worlds);
    }

    public function fortunes()
    {
        $data = [];
        $fortunes = $this->database()
            ->query('SELECT * FROM Fortune')
            ->getResultArray();

        $fortunes[] = [
            'id' => 0,
            'message' => 'Additional fortune added at request time.'
        ];

        usort($fortunes, fn($a, $b) => $a['message'] <=> $b['message']);

        $data['fortunes'] = $fortunes;

        return view('fortunes', $data);
    }
}
