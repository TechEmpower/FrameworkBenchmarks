<?php
function dbraw()
{
    global $pdo;
    static $statement;

    $statement = $statement ?? $pdo->prepare('SELECT id,randomNumber FROM World WHERE id = ?');

    if ( ! isset($_GET['queries'])) {
        $statement->execute([mt_rand(1, 10000)]);
        return json_encode($statement->fetch(PDO::FETCH_ASSOC));
    }

    $query_count = 1;
    if ($_GET['queries'] > 1) {
        $query_count = min($_GET['queries'], 500);
    }

    while ($query_count--) {
        $statement->execute([mt_rand(1, 10000)]);
        $arr[] = $statement->fetch(PDO::FETCH_ASSOC);
    }

    return json_encode($arr);
}
