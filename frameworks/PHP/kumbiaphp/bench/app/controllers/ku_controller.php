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
        KuRaw::$db->execute([mt_rand(1, 10000)]);
        echo json_encode(KuRaw::$db->fetch());
    }

    public function query($count = 1)
    {
        $count = min(max($count, 1), 500);

        while ($count--) {
            KuRaw::$db->execute([mt_rand(1, 10000)]);
            $worlds[] = KuRaw::$db->fetch();
        }
        echo json_encode($worlds);
    }

    public function update($count = 1)
    {
        $count = min(max($count, 1), 500);

        while ($count--) {
            $id = mt_rand(1, 10000);

            KuRaw::$random->execute([$id]);
            $row = ['id' => $id, 'randomNumber' => KuRaw::$random->fetchColumn()];
            $row['randomNumber'] = mt_rand(1, 10000);

            $worlds[] = $row;
        }
        KuRaw::update($worlds);

        echo json_encode($worlds);
    }
}
