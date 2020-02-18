<?php
use Workerman\Protocols\Http;

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

function router()
{
    switch (parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH)) {
        case '/plaintext':
            Http::header('Content-Type: text/plain');

            return 'Hello, World!';

        case '/json':
            Http::header('Content-Type: application/json');

            return json_encode(['message' => 'Hello, World!']);

        case '/db':
            return db();

        case '/fortune':
            // By default use 'Content-Type: text/html; charset=utf-8';
            return fortune();

        case '/query':
            return query();

        case '/update':
            return updateraw();

/*       case '/info':
            Http::header('Content-Type: text/plain');
            ob_start();
            phpinfo();
            return ob_get_clean();
 */ 
        default:
            Http::responseCode(404);
            return 'Error 404';
    }
}

function db()
{
    global $statement;
    Http::header('Content-Type: application/json');

    $statement->execute([mt_rand(1, 10000)]);

    return json_encode($statement->fetch());
}

function query()
{
    global $statement;
    Http::header('Content-Type: application/json');

    $query_count = 1;
    if ($_GET['q'] > 1) {
        $query_count = min($_GET['q'], 500);
    }

    while ($query_count--) {
        $statement->execute([mt_rand(1, 10000)]);
        $arr[] = $statement->fetch();
    }

    return json_encode($arr);
}

function updateraw()
{
    global $random, $update;
    Http::header('Content-Type: application/json');

    $query_count = 1;
    if ($_GET['q'] > 1) {
        $query_count = min($_GET['q'], 500);
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

    return json_encode($arr);
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

    return '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
            .$html.
            '</table></body></html>';
}
