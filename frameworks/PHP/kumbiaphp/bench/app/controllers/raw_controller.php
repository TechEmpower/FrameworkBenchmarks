<?php

class RawController extends AppController
{
    protected $pdo;

    protected function before_filter()
    {
        View::select(null, null);
        header('Content-type: application/json');

        $this->pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', [
            PDO::ATTR_PERSISTENT => true,
            PDO::ATTR_EMULATE_PREPARES => false
        ]);
    }

    public function index()
    {
        $statement = $this->pdo->query( 'SELECT id,randomNumber FROM World WHERE id = '. mt_rand(1, 10000) );
        echo json_encode($statement->fetch(PDO::FETCH_ASSOC));
    }

    public function queries($count = 1)
    {
        $count = min(max($count, 1), 500);
        $res = $this->pdo->prepare('SELECT id,randomNumber FROM World WHERE id = ?');
        $worlds = [];
        for ($i = 0; $i < $count; ++$i) {
            $res->execute([mt_rand(1, 10000)]);
            $worlds[] = $res->fetch(PDO::FETCH_ASSOC);
        }
        echo json_encode($worlds);
    }

    public function update($count = 1)
    {
        $count = min(max($count, 1), 500);
        $worlds = [];
        
        $sth = $this->pdo->prepare('SELECT * FROM World WHERE id = ?');
        $updateSth = $this->pdo->prepare('UPDATE World SET randomNumber = ? WHERE id = ?');
        
        for ($i = 0; $i < $count; ++$i) {
            $id = mt_rand(1, 10000);

            $sth->execute([$id]);
            $row = ['id' => $id, 'randomNumber' => $sth->fetchColumn()];
            $updateSth->execute(
                [$row['randomNumber'] = mt_rand(1, 10000), $id]
            );
            $worlds[] = $row;
        }
        echo json_encode($worlds);
    }
}