<?php

class DbController extends AppController
{
    protected function before_filter()
    {
        View::select(null, null);
        header('Content-Type: application/json');
    }

    public function index()
    {
        echo json_encode(World::byId(mt_rand(1, 10000)));
    }

    public function query($count = 1)
    {
        $count = min(max((int) $count, 1), 500);

        while ($count--) {
            $worlds[] = World::byId(mt_rand(1, 10000));
        }
        echo json_encode($worlds);
    }

    public function update($count = 1)
    {
        $count = min(max((int) $count, 1), 500);

        while ($count--) {
            $row = World::byId(mt_rand(1, 10000));
            $row->randomNumber = mt_rand(1, 10000);
            $row->update();
            $worlds[] = $row;
        }
        echo json_encode($worlds);
    }
}
