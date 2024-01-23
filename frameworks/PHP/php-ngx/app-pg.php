<?php
require 'dbraw.php';
DbRaw::init();

function db()
{
    ngx_header_set('Content-Type', 'application/json');

    DbRaw::$random->execute([mt_rand(1, 10000)]);
    echo json_encode(DbRaw::$random->fetch(), JSON_NUMERIC_CHECK);
}

function query()
{
    ngx_header_set('Content-Type', 'application/json');

    $query_count = 1;
    $params      = (int) ngx::query_args()['q'];
    if ($params > 1) {
        $query_count = min($params, 500);
    }
    while ($query_count--) {
        DbRaw::$random->execute([mt_rand(1, 10000)]);
        $arr[] = DbRaw::$random->fetch();
    }

    echo json_encode($arr, JSON_NUMERIC_CHECK);
}

function update()
{
    ngx_header_set('Content-Type', 'application/json');

    $query_count = 1;
    $params      = (int) ngx::query_args()['q'];
    if ($params > 1) {
        $query_count = min($params, 500);
    }
    while ($query_count--) {

        DbRaw::$random->execute([mt_rand(1, 10000)]);
        $row = DbRaw::$random->fetch();
        $row['randomNumber'] = mt_rand(1, 10000);

        $worlds[] = $row;
    }

    DbRaw::update($worlds);

    echo json_encode($worlds, JSON_NUMERIC_CHECK);
}

function fortune()
{
    ngx_header_set('Content-Type', 'text/html;charset=UTF-8');

    DbRaw::$fortune->execute();

    $arr    = DbRaw::$fortune->fetchAll(PDO::FETCH_KEY_PAIR);
    $arr[0] = 'Additional fortune added at request time.';
    asort($arr);

    $html = '';
    foreach ($arr as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    echo "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>";
}
