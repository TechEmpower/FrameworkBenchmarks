<?php

use Workerman\Protocols\Http;

class KuController extends AppController
{

    protected function before_filter()
    {
        View::select(null, null);
        Http::header('Content-Type: application/json');
    }

    public function index()
    {
        KuRaw::$random->execute([mt_rand(1, 10000)]);
        echo json_encode(KuRaw::$random->fetch());
    }

    public function query($count = 1)
    {
        $count = min(max((int) $count, 1), 500);

        while ($count--) {
            KuRaw::$random->execute([mt_rand(1, 10000)]);
            $worlds[] = KuRaw::$random->fetch();
        }
        echo json_encode($worlds);
    }

    public function update($count = 1)
    {
        $count = min(max((int) $count, 1), 500);

        while ($count--) {

            KuRaw::$random->execute([mt_rand(1, 10000)]);
            $row = KuRaw::$random->fetch();
            $row['randomNumber'] = mt_rand(1, 10000);

            $worlds[] = $row;
        }
        
        KuRaw::update($worlds);

        echo json_encode($worlds);
    }
}
