<?php
function dbraw($pdo)
{
    static $statement;

    $statement = $statement ?? $pdo->prepare('SELECT id,randomNumber FROM World WHERE id = ?');

    if ( ! isset($_GET['queries'])) {
        $statement->execute([mt_rand(1, 10000)]);
        echo json_encode($statement->fetch(PDO::FETCH_ASSOC));

        return;
    }

    $query_count = 1;
    if ($_GET['queries'] > 1) {
        $query_count = min($_GET['queries'], 500);
    }

    $arr = [];

    while ($query_count--) {
        $statement->execute([mt_rand(1, 10000)]);
        $arr[] = $statement->fetch(PDO::FETCH_ASSOC);
    }

    echo json_encode($arr);
}
