<?php
function updateraw()
{
    global $pdo;
    $query_count = 1;
    if ($_GET['queries'] > 1) {
        $query_count = min($_GET['queries'], 500);
    }

    $statement = $pdo->prepare('SELECT randomNumber FROM World WHERE id=?');
    $update = '';

    while ($query_count--) {
        $id = mt_rand(1, 10000);
        $statement->execute([$id]);
        $world = ['id' => $id, 'randomNumber' => $statement->fetchColumn()];
        
        $world['randomNumber'] = mt_rand(1, 10000);
        $update .= "UPDATE World SET randomNumber={$world['randomNumber']} WHERE id=$id;";

        $arr[] = $world;
    }
    $pdo->exec($update); 

    return json_encode($arr, JSON_NUMERIC_CHECK);
}
