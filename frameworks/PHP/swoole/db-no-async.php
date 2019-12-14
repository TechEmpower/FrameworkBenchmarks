<?php

/**
 * The DB test
 *
 * @param int $queries
 *
 * @return string
 */
function db(int $queries = 1) : string {
    global $pdo;
    if ( $queries === -1) {
        $statement = $pdo->prepare("SELECT id,randomNumber FROM World WHERE id=?");
        $statement->execute([mt_rand(1, 10000)]);
        return json_encode($statement->fetch(PDO::FETCH_ASSOC), JSON_NUMERIC_CHECK);
    }
    
    // Read number of queries to run from URL parameter
    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    // Create an array with the response string.
    $arr = [];
    // Define query
    $db = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id = ?');

    // For each query, store the result set values in the response array
    while ($query_count--) {
        $db->execute([mt_rand(1, 10000)]);
        $arr[] = $db->fetch(PDO::FETCH_ASSOC);
    }

    // Use the PHP standard JSON encoder.
    // http://www.php.net/manual/en/function.json-encode.php

    return json_encode($arr, JSON_NUMERIC_CHECK);
}

/**
 * The Fortunes test
 *
 * @return string
 */
function fortunes() : string {
    global $pdo;
    $fortune = [];
    $db = $pdo->prepare('SELECT id, message FROM Fortune');
    $db->execute();
    $fortune = $db->fetchAll(PDO::FETCH_KEY_PAIR);

    $fortune[0] = 'Additional fortune added at request time.';
    asort($fortune);

    $html = '';
    foreach ($fortune as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    return '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
            .$html.
            '</table></body></html>';
}

/**
 * The Updates test
 *
 * @param int $queries
 *
 * @return string
 */
function updates(int $queries) : string {
    global $pdo;
    $query_count = 1;
    if ($queries > 1) {
        $query_count = $queries > 500 ? 500 : $queries;
    }

    $statement = $pdo->prepare("SELECT randomNumber FROM World WHERE id=?");
    $update    = '';

    while ($query_count--) {
        $id = mt_rand(1, 10000);
        $statement->execute([$id]);

        $world = ["id" => $id, "randomNumber" => $statement->fetchColumn()];
        $world['randomNumber'] = mt_rand(1, 10000);
        $update .="UPDATE World SET randomNumber = {$world['randomNumber']} WHERE id = $id;";

        $arr[] = $world;
    }
    $pdo->exec($update);

    return json_encode($arr, JSON_NUMERIC_CHECK);
}
