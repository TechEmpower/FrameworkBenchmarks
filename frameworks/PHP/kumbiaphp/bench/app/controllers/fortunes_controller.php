<?php

class FortunesController extends AppController
{
    public function index()
    {
        View::select(null,'fortune');
        $data = Fortune::all('SELECT * FROM Fortune');
        $data[] = (object) ['id' => 0, 'message' => 'Additional fortune added at request time.'];
        usort($data, fn($a, $b) => $a->message <=> $b->message );
        $this->data = $data;
    }
}
