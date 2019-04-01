<?php

class RawFortuneController extends AppController
{
    public function index()
    {
        View::select(null,'raw');
        $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
            PDO::ATTR_PERSISTENT => true
        ));
        $res = $pdo->query('SELECT id, message FROM Fortune', PDO::FETCH_KEY_PAIR);
        $data = $res->fetchAll();

        $data[0] = 'Additional fortune added at request time.';
        asort($data);       
        
        $this->data = $data;
    }
}