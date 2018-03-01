<?php

class FortuneController extends AppController
{
    public function index()
    {
        View::select(null,'fortune');
        $data = World::all('SELECT id, message FROM Fortune');
        $data[] = (object) ['id' => 0,'message' => 'Additional fortune added at request time.'];
        usort($data, function($a, $b){return strcmp($a->message, $b->message);});
        $this->data = $data;
    }
}
