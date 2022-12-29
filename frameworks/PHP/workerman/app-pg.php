<?php
use Workerman\Protocols\Http\Response;
use Workerman\Protocols\Http\Request;

include 'dbraw.php';


function router(Request $request)
{
    return match($request->path()) {
        '/plaintext' => text(),
        '/json'      => json(), 
        '/db'        => db(),
        '/fortunes'  => fortune(),
        '/query'     => query($request),
        '/update'    => updateraw($request),
        // '/info'      => info(),
        default      => new Response(404, [], 'Error 404'),
    };
}

function text()
{
    return new Response(200, [
        'Content-Type' => 'text/plain',
        'Date'         => Header::$date
    ], 'Hello, World!');
}

function json()
{
    return new Response(200, [
        'Content-Type' => 'application/json',
        'Date'         => Header::$date
    ], json_encode(['message' => 'Hello, World!']));
}

function db()
{
    DbRaw::$random->execute([mt_rand(1, 10000)]);

    return new Response(200, [
        'Content-Type' => 'application/json',
        'Date'         => Header::$date
    ], json_encode(DbRaw::$random->fetch()));
}

function query($request)
{
    $query_count = 1;
    $q = (int) $request->get('q');
    if ($q > 1) {
        $query_count = min($q, 500);
    }

    while ($query_count--) {
        DbRaw::$random->execute([mt_rand(1, 10000)]);
        $arr[] = DbRaw::$random->fetch();
    }

    return new Response(200, [
        'Content-Type' => 'application/json',
        'Date'         => Header::$date
    ], json_encode($arr));
}

function updateraw($request)
{
    $query_count = 1;
    $q = (int) $request->get('q');
    if ($q > 1) {
        $query_count = min($q, 500);
    }

    while ($query_count--) {

        DbRaw::$random->execute([mt_rand(1, 10000)]);
        $row = DbRaw::$random->fetch();
        $row['randomNumber'] = mt_rand(1, 10000);

        $worlds[] = $row;
    }

    DbRaw::update($worlds);

    return new Response(200, [
        'Content-Type' => 'application/json',
        'Date'         => Header::$date
    ], json_encode($worlds));
}

function fortune()
{
    DbRaw::$fortune->execute();

    $arr    = DbRaw::$fortune->fetchAll(PDO::FETCH_KEY_PAIR);
    $arr[0] = 'Additional fortune added at request time.';
    asort($arr);

    $html = '';
    foreach ($arr as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    return new Response(200, [
        'Date'         => Header::$date
    ], "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>"
    );
}

/* function info()
{
    ob_start();
    phpinfo();
    return new Response(200, ['Content-Type' => 'text/plain'], ob_get_clean());
}
 */
