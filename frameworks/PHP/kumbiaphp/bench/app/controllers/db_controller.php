<?php

class DbController extends AppController
{
    protected function before_filter()
    {
        View::select(null, null);
        header('Content-type: application/json');
    }

    public function index()
    {
        echo json_encode(World::byId(mt_rand(1, 10000)));
    }

    public function queries($count = 1)
    {
        $count = is_numeric($count) ? min(max($count, 1), 500) : 1;
        $worlds = [];
        for ($i = 0; $i < $count; ++$i) {
            $worlds[] = World::byId(mt_rand(1, 10000));
        }
        echo json_encode($worlds);
    }

    public function update($count = 1)
    {
        $count = is_numeric($count) ? min(max($count, 1), 500) : 1;
        $worlds = [];
        for ($i = 0; $i < $count; ++$i) {
            $row = World::byId(mt_rand(1, 10000));
            $row->randomNumber = mt_rand(1, 10000);
            $row->update();
            $worlds[] = $row;
        }
        echo json_encode($worlds);
    }
}
