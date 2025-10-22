<?php

class KuController extends AppController
{

    protected function before_filter()
    {
        View::select(null, null);
        header('Content-Type: application/json');
    }

    public function index()
    {
        KuRaw::$random->execute([mt_rand(1, 10000)]);
        echo json_encode(KuRaw::$random->fetch());
    }

    public function query($count = 1)
    {
        $count = min(max((int) $count, 1), 500);
        $random = KuRaw::$random;

        while ($count--) {
            $random->execute([mt_rand(1, 10000)]);
            $worlds[] = $random->fetch();
        }
        echo json_encode($worlds);
    }

    public function update($count = 1)
    {
        $count = min(max((int) $count, 1), 500);
        $random = KuRaw::$random;

        while ($count--) {

            $random->execute([mt_rand(1, 10000)]);
            $row = $random->fetch();
            $row['randomNumber'] = mt_rand(1, 10000);

            $worlds[] = $row;
        }
        
        KuRaw::update($worlds);

        echo json_encode($worlds);
    }
}
