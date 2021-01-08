<?php

$pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass',
            [PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
            PDO::ATTR_EMULATE_PREPARES   => false]
);

$statement = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
$fortune   = $pdo->prepare('SELECT id,message FROM Fortune');
$update    = $pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');

function db()
{
    global $statement;
    ngx_header_set('Content-Type', 'application/json');

    $statement->execute([mt_rand(1, 10000)]);
    echo json_encode($statement->fetch(), JSON_NUMERIC_CHECK);
}

function query()
{
    global $statement;
    ngx_header_set('Content-Type', 'application/json');

    $query_count = 1;
    $params      = (int) ngx::query_args()['q'];
    if ($params > 1) {
        $query_count = min($params, 500);
    }
    while ($query_count--) {
        $statement->execute([mt_rand(1, 10000)]);
        $arr[] = $statement->fetch();
    }

    echo json_encode($arr, JSON_NUMERIC_CHECK);
}

function update()
{
    global $statement, $update;
    ngx_header_set('Content-Type', 'application/json');

    $query_count = 1;
    $params      = (int) ngx::query_args()['q'];
    if ($params > 1) {
        $query_count = min($params, 500);
    }
    while ($query_count--) {
        $id = mt_rand(1, 10000);
        $statement->execute([$id]);

        $world = $statement->fetch();
        $update->execute(
            [$world['randomNumber'] = mt_rand(1, 10000), $id]
        );
        $arr[] = $world;
    }

    echo json_encode($arr, JSON_NUMERIC_CHECK);
}

function fortune()
{
    global $fortune;
    ngx_header_set('Content-Type', 'text/html;charset=UTF-8');

    $fortune->execute();

    $arr    = $fortune->fetchAll(PDO::FETCH_KEY_PAIR);
    $arr[0] = 'Additional fortune added at request time.';
    asort($arr);

    $html = '';
    foreach ($arr as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    echo "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>";
}
