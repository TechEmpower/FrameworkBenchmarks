<?php

$pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass');


function db()
{
    global $pdo;
    ngx_header_set('Content-Type', 'application/json');

    $statement = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');

    $statement->execute([mt_rand(1, 10000)]);
    echo json_encode($statement->fetch(PDO::FETCH_ASSOC), JSON_NUMERIC_CHECK);
}

function query()
{
    global $pdo;
    ngx_header_set('Content-Type', 'application/json');

    $statement = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');

    $query_count = 1;
    $params      = ngx::query_args()['queries'];
    if ($params > 1) {
        $query_count = min($params, 500);
    }

    while ($query_count--) {
        $statement->execute([mt_rand(1, 10000)]);
        $arr[] = $statement->fetch(PDO::FETCH_ASSOC);
    }

    echo json_encode($arr, JSON_NUMERIC_CHECK);
}

function update()
{
    global $pdo;
    ngx_header_set('Content-Type', 'application/json');

    $query_count = 1;
    $params      = ngx::query_args()['queries'];
    if ($params > 1) {
        $query_count = min($params, 500);
    }

    $statement       = $pdo->prepare('SELECT randomNumber FROM World WHERE id=?');
    $updateStatement = $pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');

    while ($query_count--) {
        $id = mt_rand(1, 10000);
        $statement->execute([$id]);

        $world = ['id' => $id, 'randomNumber' => $statement->fetchColumn()];
        $updateStatement->execute(
            [$world['randomNumber'] = mt_rand(1, 10000), $id]
        );

        $arr[] = $world;
    }

    echo json_encode($arr, JSON_NUMERIC_CHECK);
}

function fortune()
{
    global $pdo;
    ngx_header_set('Content-Type', 'text/html;charset=UTF-8');

    $fortune = $pdo->prepare('SELECT id,message FROM Fortune');
    $fortune->execute();

    $arr    = $fortune->fetchAll(PDO::FETCH_KEY_PAIR);
    $arr[0] = 'Additional fortune added at request time.';
    asort($arr);

    $html = '';
    foreach ($arr as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    echo    '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>',
            $html,
            '</table></body></html>';
}
