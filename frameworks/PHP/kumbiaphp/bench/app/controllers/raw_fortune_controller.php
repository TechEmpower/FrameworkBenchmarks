<?php

class RawFortuneController extends AppController
{
    public function index()
    {
        View::select(null,'raw');
        $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
            PDO::ATTR_PERSISTENT => true
        ));
        $res = $pdo->query('SELECT id, message FROM Fortune', PDO::FETCH_ASSOC);
        $data = $res->fetchAll();
        
        array_push($data, array('id'=> 0, 'message' => 'Additional fortune added at request time.'));
        usort($data, function($left, $right) {
            return strcmp($left['message'], $right['message']);
        });
        $this->data = $data;
    }
}