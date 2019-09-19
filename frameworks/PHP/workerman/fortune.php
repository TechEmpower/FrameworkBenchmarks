<?php
function fortune()
{
    global $pdo;
    static $statement;
    $statement = $statement ?? $pdo->prepare('SELECT id,message FROM Fortune');
    $statement->execute();

    $arr       = $statement->fetchAll(PDO::FETCH_KEY_PAIR);
    $arr[0]    = 'Additional fortune added at request time.';
    asort($arr);

    $html = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>';
    foreach ($arr as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>{$id}</td><td>{$message}</td></tr>";
    }
    return $html . '</table></body></html>';
}
