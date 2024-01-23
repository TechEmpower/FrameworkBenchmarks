<?php

class RaController extends AppController
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
        $statement = $this->pdo->query( 'SELECT * FROM World WHERE id='. mt_rand(1, 10000) );
        echo json_encode($statement->fetch(PDO::FETCH_ASSOC));
    }

    public function query($count = 1)
    {
        $count = min(max((int) $count, 1), 500);
        $res = $this->pdo->prepare('SELECT * FROM World WHERE id=?');

        while ($count--) {
            $res->execute([mt_rand(1, 10000)]);
            $worlds[] = $res->fetch(PDO::FETCH_ASSOC);
        }
        echo json_encode($worlds);
    }

    public function update($count = 1)
    {
        $count = min(max((int) $count, 1), 500);

        $this->pdo->setAttribute(PDO::ATTR_EMULATE_PREPARES, false);
        $sth = $this->pdo->prepare('SELECT id, randomNumber FROM World WHERE id=?');
        $updateStatement = $this->pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');

        while ($count--) {
            $id = mt_rand(1, 10000);

            $sth->execute([$id]);
            $row = $sth->fetch(PDO::FETCH_ASSOC);
            $updateStatement->execute(
                [$row['randomNumber'] = mt_rand(1, 10000), $id]
            );

            $worlds[] = $row;
        }

        echo json_encode($worlds);
    }
}
