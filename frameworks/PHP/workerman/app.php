<?php
use Workerman\Protocols\Http\Response;
use Workerman\Protocols\Http\Request;

function init()
{
    global $statement, $fortune, $random, $update;
    $pdo = new PDO('mysql:host=tfb-database;dbname=hello_world',
        'benchmarkdbuser', 'benchmarkdbpass',
        [PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
        PDO::ATTR_EMULATE_PREPARES    => false]
    );
    $statement = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
    $fortune   = $pdo->prepare('SELECT id,message FROM Fortune');
    $random    = $pdo->prepare('SELECT randomNumber FROM World WHERE id=?');
    $update    = $pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');
}

function router(Request $request)
{
    switch ($request->path()) {
        case '/plaintext':
            return new Response(200, [
                'Content-Type' => 'text/plain',
                'Date'         => Header::$date
            ], 'Hello, World!');

        case '/json':
            return new Response(200, [
                'Content-Type' => 'application/json',
                'Date'         => Header::$date
            ], json_encode(['message' => 'Hello, World!']));

        case '/db':
            return db();

        case '/fortune':
            // By default use 'Content-Type: text/html; charset=utf-8';
            return fortune();

        case '/query':
            return query($request);

        case '/update':
            return updateraw($request);
/*
       case '/info':
            ob_start();
            phpinfo();
            return new Response(200, ['Content-Type' => 'text/plain'], ob_get_clean());
*/
        default:
            return new Response(404, [], 'Error 404');
    }
}

function db()
{
    global $statement;

    $statement->execute([mt_rand(1, 10000)]);

    return new Response(200, [
        'Content-Type' => 'application/json',
        'Date'         => Header::$date
    ], json_encode($statement->fetch()));
}

function query($request)
{
    global $statement;

    $query_count = 1;
    $q = $request->get('q');
    if ($q > 1) {
        $query_count = min($q, 500);
    }

    while ($query_count--) {
        $statement->execute([mt_rand(1, 10000)]);
        $arr[] = $statement->fetch();
    }

    return new Response(200, [
        'Content-Type' => 'application/json',
        'Date'         => Header::$date
    ], json_encode($arr));
}

function updateraw($request)
{
    global $random, $update;

    $query_count = 1;
    $q = $request->get('q');
    if ($q > 1) {
        $query_count = min($q, 500);
    }

    while ($query_count--) {
        $id = mt_rand(1, 10000);
        $random->execute([$id]);
        $world = ['id' => $id, 'randomNumber' => $random->fetchColumn()];
        $update->execute(
            [$world['randomNumber'] = mt_rand(1, 10000), $id]
        );

        $arr[] = $world;
    }

    // $pdo->beginTransaction();
    // foreach($arr as $world) {
    //     $update->execute([$world['randomNumber'], $world['id']]);
    // }
    // $pdo->commit();
    return new Response(200, [
        'Content-Type' => 'application/json',
        'Date'         => Header::$date
    ], json_encode($arr));
}

function fortune()
{
    global $fortune;

    $fortune->execute();

    $arr    = $fortune->fetchAll(PDO::FETCH_KEY_PAIR);
    $arr[0] = 'Additional fortune added at request time.';
    asort($arr);

    $html = '';
    foreach ($arr as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    return new Response(200, [
        'Date'         => Header::$date
    ], '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
        .$html.
        '</table></body></html>');
}
