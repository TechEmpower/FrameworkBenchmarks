<?php
require_once '/ngx_php7/t/lib/mysql.php';

define('DB_HOST', gethostbyname('tfb-database'));
define('DB_PORT', '3306');
define('DB_USER', 'benchmarkdbuser');
define('DB_PASS', 'benchmarkdbpass');
define('DB_NAME', 'hello_world');


function fortune()
{
    ngx_header_set('Content-Type', 'text/html;charset=UTF-8');
    $my = new php\ngx\mysql();
    yield from $my->connect(DB_HOST, DB_PORT, DB_USER, DB_PASS, DB_NAME);
    $ret = yield from $my->query2('SELECT id,message FROM Fortune');

    foreach ($ret as $row) {
        $arr[$row['id']] = $row['message'];
    }
    $arr[0] = 'Additional fortune added at request time.';
    asort($arr);

    $html = '';
    foreach ($arr as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }
    echo "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>";

    unset($my);
}

function query()
{
    ngx_header_set('Content-Type', 'application/json');
    $my = new php\ngx\mysql();
    yield from $my->connect(DB_HOST, DB_PORT, DB_USER, DB_PASS, DB_NAME);
    $query_count = 1;
    $params      = (int) ngx::query_args()['q'];
    if ($params > 1) {
        $query_count = min($params, 500);
    }

    while ($query_count--) {
        $arr[] = (yield from $my->query2('SELECT id,randomNumber FROM World WHERE id = '.mt_rand(1, 10000)))[0];
    }
    unset($my);
    echo json_encode($arr);
}

function db()
{
    ngx_header_set('Content-Type', 'application/json');

    $my = new php\ngx\mysql();
    yield from $my->connect(DB_HOST, DB_PORT, DB_USER, DB_PASS, DB_NAME);

    echo json_encode(
        (yield from $my->query2('SELECT id,randomNumber FROM World WHERE id = '.mt_rand(1, 10000)))[0]
    );
    unset($my);
}

function update()
{
    ngx_header_set('Content-Type', 'application/json');
    $my = new php\ngx\mysql();
    yield from $my->connect(DB_HOST, DB_PORT, DB_USER, DB_PASS, DB_NAME);
    $query_count = 1;
    $params      = (int) ngx::query_args()['q'];
    if ($params > 1) {
        $query_count = min($params, 500);
    }

    while ($query_count--) {
        $id     = mt_rand(1, 10000);
        $world  = (yield from $my->query2("SELECT id,randomNumber FROM World WHERE id = $id"))[0];
        
        $world['randomNumber'] = mt_rand(1, 10000);
        yield from $my->query2("UPDATE World SET randomNumber = {$world['randomNumber']} WHERE id = $id");
        $arr[] = $world;
    }
    unset($my);
    echo json_encode($arr);
}
