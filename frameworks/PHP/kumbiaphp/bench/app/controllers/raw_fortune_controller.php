<?php

class RawFortuneController extends AppController
{
    public function index()
    {
        View::select(null,'raw');
        $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', [
            PDO::ATTR_PERSISTENT => true
        ]);
        $data = $pdo->query('SELECT * FROM Fortune')->fetchAll();

        $data[0] = 'Additional fortune added at request time.';
        asort($data);
        
        $this->data = $data;
    }
}
