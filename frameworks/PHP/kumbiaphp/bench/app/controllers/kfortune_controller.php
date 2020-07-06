<?php

class KfortuneController extends AppController
{

    protected function before_filter()
    {
        View::select(null, 'raw');
    }

    public function index()
    {
        KuRaw::$fortune->execute();
        $arr    = KuRaw::$fortune->fetchAll(PDO::FETCH_KEY_PAIR);
        $arr[0] = 'Additional fortune added at request time.';
        asort($arr);

        $this->data = $arr;
    }
}
