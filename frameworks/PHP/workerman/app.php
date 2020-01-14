<?php
function db()
{
    global $statement;

    $statement->execute([mt_rand(1, 10000)]);
    return json_encode($statement->fetch(), JSON_NUMERIC_CHECK);
}

function query()
{
    global $statement;
    
    $query_count = 1;
    if ($_GET['q'] > 1) {
        $query_count = min($_GET['q'], 500);
    }

    while ($query_count--) {
        $statement->execute([mt_rand(1, 10000)]);
        $arr[] = $statement->fetch();
    }

    return json_encode($arr, JSON_NUMERIC_CHECK);
}

function updateraw()
{
    global $pdo, $random, $update;
    $query_count = 1;
    if ($_GET['q'] > 1) {
        $query_count = min($_GET['q'], 500);
    }

    while ($query_count--) {
        $id = mt_rand(1, 10000);
        $random->execute([$id]);
        $world = ['id' => $id, 'randomNumber' => $random->fetchColumn()];
        $update->execute(
            [$world['randomNumber'] = mt_rand(1, 10000), $id]
        );

        $arr[] = $world;
    }
    
    // $pdo->beginTransaction();
    // foreach($arr as $world) {
    //     $update->execute([$world['randomNumber'], $world['id']]);
    // }
    // $pdo->commit();

    return json_encode($arr, JSON_NUMERIC_CHECK);
}

function fortune()
{
    global $fortune;

    //$fortune = $pdo->prepare('SELECT id,message FROM Fortune');
    $fortune->execute();

    $arr    = $fortune->fetchAll(PDO::FETCH_KEY_PAIR);
    $arr[0] = 'Additional fortune added at request time.';
    asort($arr);

    $html = '';
    foreach ($arr as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    return '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
            .$html.
            '</table></body></html>';
}
