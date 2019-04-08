<?php

class RawFortuneController extends AppController
{
    public function index()
    {
        View::select(null,'raw');
        $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', [
            PDO::ATTR_PERSISTENT => true
        ]);
        $data = $pdo->query('SELECT id, message FROM Fortune', PDO::FETCH_KEY_PAIR)->fetchAll();

        $data[0] = 'Additional fortune added at request time.';
        asort($data);       
        
        $this->data = $data;
    }
}