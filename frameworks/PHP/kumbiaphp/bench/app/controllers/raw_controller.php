<?php

class RawController extends AppController
{
    protected $pdo;

    protected function before_filter()
    {
        View::select(null, null);
        header('Content-Type: application/json');

        $this->pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', [
            PDO::ATTR_PERSISTENT => true
        ]);
    }

    public function index()
    {
        $statement = $this->pdo->query( 'SELECT id,randomNumber FROM World WHERE id='. mt_rand(1, 10000) );
        echo json_encode($statement->fetch(PDO::FETCH_ASSOC), JSON_NUMERIC_CHECK);
    }

    public function query($count = 1)
    {
        $count = min(max($count, 1), 500);
        $res = $this->pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');

        while ($count--) {
            $res->execute([mt_rand(1, 10000)]);
            $worlds[] = $res->fetch(PDO::FETCH_ASSOC);
        }
        echo json_encode($worlds, JSON_NUMERIC_CHECK);
    }

    public function update($count = 1)
    {
        $count = min(max($count, 1), 500);
        
        $sth = $this->pdo->prepare('SELECT randomNumber FROM World WHERE id=?');
        $updateSth = $this->pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');
        
        while ($count--) {
            $id = mt_rand(1, 10000);

            $sth->execute([$id]);
            $row = ['id' => $id, 'randomNumber' => $sth->fetchColumn()];
            $updateSth->execute(
                [$row['randomNumber'] = mt_rand(1, 10000), $id]
            );
            $worlds[] = $row;
        }
        echo json_encode($worlds, JSON_NUMERIC_CHECK);
    }
}