<?php

class RfortuneController extends AppController
{
    public function index()
    {
        View::select(null,'raw');
        $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', [
            PDO::ATTR_PERSISTENT         => true,
            PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_KEY_PAIR
        ]);
        $data = $pdo->query('SELECT * FROM Fortune')->fetchAll();

        $data[0] = 'Additional fortune added at request time.';
        asort($data);
        
        $this->data = $data;
    }
}
