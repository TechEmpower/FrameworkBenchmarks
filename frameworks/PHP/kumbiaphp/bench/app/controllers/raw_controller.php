<?php

class RawController extends AppController
{
    protected $pdo;

    protected function before_filter()
    {
        View::select(null, null);
        header('Content-type: application/json');

        $this->pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
            PDO::ATTR_PERSISTENT => true
        ));
    }

    public function index()
    {
        $res = $this->pdo->prepare('SELECT randomNumber FROM World WHERE id = ?');
        $id = mt_rand(1, 10000);
        $res->execute(array($id));
        echo json_encode(['id' => $id, 'randomNumber' => $res->fetchColumn()]);
    }

    public function queries($count = 1)
    {
        //$queries = ($queries < 1) ? 1 : (($queries > 500) ? 500 : $queries);
        $count = is_numeric($count) ? min(max($count, 1), 500) : 1;
        $res = $this->pdo->prepare('SELECT randomNumber FROM World WHERE id = ?');
        $worlds = [];
        for ($i = 0; $i < $count; ++$i) {
            $id = mt_rand(1, 10000);
            $res->execute(array($id));
            $worlds[] = array('id' => $id, 'randomNumber' => $res->fetchColumn());
        }
        echo json_encode($worlds);
    }

    public function update($count = 1)
    {
        $count = is_numeric($count) ? min(max($count, 1), 500) : 1;
        $worlds = [];
        
        $sth = $this->pdo->prepare('SELECT * FROM World WHERE id = ?');
        $updateSth = $this->pdo->prepare('UPDATE World SET randomNumber = ? WHERE id = ?');
        
        for ($i = 0; $i < $count; ++$i) {
            $id = mt_rand(1, 10000);
            $randomNumber = mt_rand(1, 10000);

            $sth->execute(array($id));
            $row = ['id' => $id, 'randomNumber' => $updateSth->fetchColumn()];
            $row['randomNumber'] = $randomNumber;
            $updateSth->execute([$randomNumber, $id]);
            $worlds[] = $row;
        }
        echo json_encode($worlds);
    }
}