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
