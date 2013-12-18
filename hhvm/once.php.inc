<?php

class Benchmark {
    var $pdo;

    public function setup_db()
    {
        $this->pdo = new PDO('mysql:host=localhost;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
            PDO::ATTR_PERSISTENT => true,
            PDO::MYSQL_ATTR_INIT_COMMAND => "SET NAMES 'utf8'"
        ));
    }

    public function bench_json()
    {
        header('Content-Type: application/json');
        echo json_encode(array('message' => 'Hello, World!'));
    }

    public function bench_plaintext()
    {
        header('Content-Type: text/plain; charset=utf-8');
        echo 'Hello, World!';
    }

    public function bench_db()
    {
        $this->setup_db();

        // Create an array with the response string.
        $arr = array();
        $id = mt_rand(1, 10000);

        // Define query
        $statement = $this->pdo->prepare('SELECT randomNumber FROM World WHERE id = :id');
        $statement->bindParam(':id', $id, PDO::PARAM_INT);
        $statement->execute();

        // Store result in array.
        $arr = array('id' => $id, 'randomNumber' => $statement->fetchColumn());
        $id = mt_rand(1, 10000);

        // Send the required parameters
        header('Content-Type: application/json');
        echo json_encode($arr);
    }

    public function bench_queries($query_count=1)
    {
        $this->setup_db();

        // Create an array with the response string.
        $arr = array();
        $id = mt_rand(1, 10000);

        // Define query
        $statement = $this->pdo->prepare('SELECT randomNumber FROM World WHERE id = :id');
        $statement->bindParam(':id', $id, PDO::PARAM_INT);

        // For each query, store the result set values in the response array
        while (0 < $query_count--) {
          $statement->execute();

          // Store result in array.
          $arr[] = array('id' => $id, 'randomNumber' => $statement->fetchColumn());
          $id = mt_rand(1, 10000);
        }

        // Send the required parameters
        header('Content-Type: application/json');
        echo json_encode($arr);
    }

    public function bench_updates($query_count)
    {
        $this->setup_db();

        // Create an array with the response string.
        $arr = array();
        $id = mt_rand(1, 10000);
        $randomNumber = mt_rand(1, 1000);

        // Define query
        $statement = $this->pdo->prepare('SELECT randomNumber FROM World WHERE id = :id');
        $statement->bindParam(':id', $id, PDO::PARAM_INT);

        $updateStatement = $this->pdo->prepare('UPDATE World SET randomNumber = :randomNumber WHERE id = :id');
        $updateStatement->bindParam(':id', $id, PDO::PARAM_INT);
        $updateStatement->bindParam(':randomNumber', $randomNumber, PDO::PARAM_INT);

        // For each query, store the result set values in the response array
        while (0 < $query_count--) {
          $statement->execute();

          // Store result in array.
          $world = array('id' => $id, 'randomNumber' => $statement->fetchColumn());
          $world['randomNumber'] = $randomNumber;
          $updateStatement->execute();

          $arr[] = $world;
          $id = mt_rand(1, 10000);
          $randomNumber = mt_rand(1, 10000);
        }

        // Send the required parameters
        header('Content-Type: application/json');
        echo json_encode($arr);
    }

    public function bench_fortunes()
    {
        $this->setup_db();

        // Define query
        $statement = $this->pdo->query( 'SELECT id, message FROM Fortune' );

        // Store result in array.
        $arr = $statement->fetchAll(PDO::FETCH_KEY_PAIR);
        $arr[0] = 'Additional fortune added at request time.';

        asort($arr);
        header("Content-Type: text/html; charset=utf-8");
        echo <<<EOM
<!DOCTYPE html>
<html>
<head>
<title>Fortunes</title>
</head>
<body>
<table>
<tr>
<th>id</th>
<th>message</th>
</tr>
EOM;
        foreach ( $arr as $id => &$fortune ) {
            echo '<tr>';
            echo '<td>'.htmlspecialchars($id, ENT_QUOTES, 'utf-8').'</td>';
            echo '<td>'.htmlspecialchars($fortune, ENT_QUOTES, 'utf-8').'</td>';
            echo '</tr>';
        }
echo <<<EOM
</table>
</body>
</html>
EOM;
    }
}
