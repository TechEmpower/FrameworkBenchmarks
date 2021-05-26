<?php
use Psr\Http\Message\ServerRequestInterface as Request;
use React\Http\Message\Response;

function init()
{
    global $world, $fortune, $update;
    $pdo = new PDO(
        'mysql:host=tfb-database;dbname=hello_world',
        'benchmarkdbuser',
        'benchmarkdbpass',
        [
            PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
            PDO::ATTR_EMULATE_PREPARES   => false
        ]
    );
    $world   = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
    $update  = $pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');
    $fortune = $pdo->prepare('SELECT id,message FROM Fortune');
    $fortune->setFetchMode(PDO::FETCH_KEY_PAIR);
}

function router(Request $request)
{
    return match($request->getUri()->getPath()) {
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
        'Content-Type' => 'text/plain'
    ], 'Hello, World!');
}

function json()
{
    return new Response(200, [
        'Content-Type' => 'application/json'
    ], json_encode(['message' => 'Hello, World!']));
}

function db()
{
    global $world;

    $world->execute([mt_rand(1, 10000)]);

    return new Response(200, [
        'Content-Type' => 'application/json'
    ], json_encode($world->fetch()));
}

function query($request)
{
    global $world;

    $query_count = 1;
    $q = (int) $request->getQueryParams()['q'];
    if ($q > 1) {
        $query_count = min($q, 500);
    }                                                          

    while ($query_count--) {
        $world->execute([mt_rand(1, 10000)]);
        $arr[] = $world->fetch();
    }

    return new Response(200, [
        'Content-Type' => 'application/json'
    ], json_encode($arr));
}

function updateraw($request)
{
    global $world, $update;

    $query_count = 1;
    $q = (int) $request->getQueryParams()['q'];
    if ($q > 1) {
        $query_count = min($q, 500);
    }

    while ($query_count--) {
        $id = mt_rand(1, 10000);
        $world->execute([$id]);
        $item = $world->fetch();
        $update->execute(
            [$item['randomNumber'] = mt_rand(1, 10000), $id]
        );

        $arr[] = $item;
    }

    // $pdo->beginTransaction();
    // foreach($arr as $world) {
    //     $update->execute([$world['randomNumber'], $world['id']]);
    // }
    // $pdo->commit();
    return new Response(200, [
        'Content-Type' => 'application/json'
    ], json_encode($arr));
}

function fortune()
{
    global $fortune;

    $fortune->execute();

    $arr    = $fortune->fetchAll();
    $arr[0] = 'Additional fortune added at request time.';
    asort($arr);

    $html = '';
    foreach ($arr as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    return new Response(200, [
        'Content-Type' => 'text/html; charset=UTF-8',
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