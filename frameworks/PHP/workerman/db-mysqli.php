<?php

//$db = new mysqli('p:tfb-database', 'benchmarkdbuser', 'benchmarkdbpass', 'hello_world');


function db()
{
    global $db;

    $statement = $db->prepare('SELECT id,randomNumber FROM World WHERE id=?');

    if ( ! isset($_GET['queries'])) {
        $id = mt_rand(1, 10000);
        $statement->bind_param('i', $id);
        $statement->execute();
        return json_encode($statement->get_result()->fetch_assoc(), JSON_NUMERIC_CHECK);
    }

    $query_count = 1;
    if ($_GET['queries'] > 1) {
        $query_count = min($_GET['queries'], 500);
    }

    while ($query_count--) {
        $id = mt_rand(1, 10000);
        $statement->bind_param('i', $id);
        $statement->execute();
        $arr[] = $statement->get_result()->fetch_assoc();
    }

    return json_encode($arr, JSON_NUMERIC_CHECK);
}

function fortune()
{
    global $db;

    $arr    = $db->query('SELECT id,message FROM Fortune')->fetch_all(MYSQLI_NUM);
    $arr[] = [0 => 0, 1 => 'Additional fortune added at request time.'];
    usort($arr, function($a, $b){return strcmp($a[1], $b[1]);});

    $html = '';
    foreach ($arr as $item) {
        $message = htmlspecialchars($item[1], ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$item[0]</td><td>$message</td></tr>";
    }

    return '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
            .$html.
            '</table></body></html>';
}

function update()
{
    global $db;

    $query_count = 1;
    if ($_GET['queries'] > 1) {
        $query_count = min($_GET['queries'], 500);
    }

    $statement       = $db->prepare('SELECT randomNumber FROM World WHERE id=?');
    $updateStatement = $db->prepare('UPDATE World SET randomNumber=? WHERE id=?');

    while ($query_count--) {
        $id = mt_rand(1, 10000);
        $statement->bind_param('i', $id);
        $statement->execute();

        $world = ['id' => $id, 'randomNumber' => $statement->get_result()->fetch_row()];
        
        $update = mt_rand(1, 10000);
        $updateStatement->bind_param('ii',$update, $id);
        $updateStatement->execute();

        $arr[] = ['id' => $id, 'randomNumber' => $update];
    }

    return json_encode($arr, JSON_NUMERIC_CHECK);
}
