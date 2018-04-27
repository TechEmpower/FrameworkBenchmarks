<?php
$server = new swoole_http_server('0.0.0.0', 8080, SWOOLE_BASE);
$server->set(array(
    'worker_num' => NUMCORES
));
$server->on('request', function ($req, $res) {

    switch ($req->server['request_uri'])
    {
        case "/json":
            $res->header('Content-type', 'application/json');
            $res->end(json_encode(array('message' => 'Hello, World!')));
            break;

        case "/plaintext":
            $res->header('Content-Type', 'text/plain');
            $res->end('Hello, World!');
            break;

        case "/db":
            $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
                PDO::ATTR_PERSISTENT => true
            ));
            // Read number of queries to run from URL parameter
            $query_count = 1;
            if (isset($_GET['queries']) && $_GET['queries'] > 0)
                $query_count = $_GET['queries'] > 500 ? 500 : $_GET['queries'];

            // Create an array with the response string.
            $arr = array();
            // Define query
            $statement = $pdo->prepare('SELECT randomNumber FROM World WHERE id = ?');
            // For each query, store the result set values in the response array
            while (0 < $query_count--)
            {
                $id = mt_rand(1, 10000);
                $statement->execute(array($id));

                // Store result in array.
                $arr[] = array('id' => $id, 'randomNumber' => $statement->fetchColumn());
            }
            // Use the PHP standard JSON encoder.
            // http://www.php.net/manual/en/function.json-encode.php
            if (count($arr) === 1)
                $arr = $arr[0];

            $res->header('Content-Type', 'application/json');
            $res->end(json_encode($arr));
            break;

        case "/fortunes":
            $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world;charset=utf8', 'benchmarkdbuser', 'benchmarkdbpass', array(
                PDO::ATTR_PERSISTENT => true
            ));
            // Define query
            $statement = $pdo->query('SELECT id, message FROM Fortune');

            // Store result in array.
            $arr = $statement->fetchAll(PDO::FETCH_KEY_PAIR);
            $arr[0] = 'Additional fortune added at request time.';
            asort($arr);

            $html = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
            foreach ($arr as $id => $fortune)
                $html .= "<tr><td>" . $id . "</td><td>" . htmlspecialchars($fortune, ENT_QUOTES, 'UTF-8') . "</td></tr>";

            $html .= "</table></body></html>";

            $res->header('Content-Type', 'text/html; charset=utf-8');
            $res->end($html);
            break;

        case "/updates":
            $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
                PDO::ATTR_PERSISTENT => true
            ));

            $query_count = 1;
            if (isset($_GET['queries']) && $_GET['queries'] > 0)
            {
                $query_count = $_GET['queries'] > 500 ? 500 : $_GET['queries'];
            }

            $arr = array();
            $statement = $pdo->prepare('SELECT randomNumber FROM World WHERE id = ?');
            $updateStatement = $pdo->prepare('UPDATE World SET randomNumber = ? WHERE id = ?');

            while (0 < $query_count--)
            {
                $id = mt_rand(1, 10000);
                $randomNumber = mt_rand(1, 10000);
                $statement->execute(array($id));

                // Store result in array.
                $world = array('id' => $id, 'randomNumber' => $statement->fetchColumn());
                $world['randomNumber'] = $randomNumber;
                $updateStatement->execute(array($randomNumber, $id));

                $arr[] = $world;
            }

            $res->header('Content-Type', 'application/json');
            $res->end(json_encode($arr));
            break;
    }

});
$server->start();
