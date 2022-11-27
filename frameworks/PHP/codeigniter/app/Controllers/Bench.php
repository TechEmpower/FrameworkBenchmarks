<?php

namespace App\Controllers;

class Bench extends BaseController
{
    public function plaintext()
    {
        $this->response->setContentType('text/plain');
        return $this->response->setBody('Hello, World!');
    }

    public function json()
    {
        return $this->response->setJSON(['message' => 'Hello, World!']);
    }

    public function db()
    {
        $worlds = $this->db
            ->query('SELECT * FROM World WHERE id = ?', array(mt_rand(1, 10000)))
            ->getRow();

        return $this->response->setJSON($worlds);
    }

    public function queries($queries = 1)
    {
        $worlds = [];
        $queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $this->db
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
            $world = $this->db
                ->query('SELECT * FROM World WHERE id = ?', [$id])
                ->getRow();
            
            $world->randomNumber = mt_rand(1, 10000);
            $this->db
                ->query('UPDATE World SET randomNumber=? WHERE id=?', [$world->randomNumber, $id]);
            $worlds[] = $world;
        }

        return $this->response->setJSON($worlds);
    }

    public function fortunes()
    {
        $fortunes = $this->db
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
