<?php

use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;

require_once __DIR__.'/../vendor/autoload.php';

$app = new Silex\Application();

// Test 1: JSON serialization
$app->get('/json', function() {
    // The following code would be handier:
    //   return new JsonResponse(array("message" => "Hello World!"));
    //
    // But JsonResponse does some checks that are unneeded for this benchmark
    // and therefore, a plain Response object is faster.
    return new Response(json_encode(array('message' => 'Hello World!')), 200, array('Content-Type' => 'application/json'));
});

// Test 2: Single database query
$app->get('/db', function() {
    $db = new mysqli('172.16.98.120', 'benchmarkdbuser', 'benchmarkdbpass', 'hello_world');
    $row = mysqli_query($db, 'SELECT id, randomNumber FROM World WHERE id = '.rand(1, 10000));

    return new Response(json_encode(mysqli_fetch_assoc($row)), 200, array('Content-Type' => 'application/json'));
});

// Test 3: Multiple database queries
$app->get('/queries', function(Request $request) {
    $queries = max(1, min($request->query->get('queries'), 500));
    $db = new mysqli('172.16.98.120', 'benchmarkdbuser', 'benchmarkdbpass', 'hello_world');

    for ($i=0; $i<$queries; $i++) {
        $rows[] = mysqli_fetch_assoc(mysqli_query($db, 'SELECT id, randomNumber FROM World WHERE id = '.rand(1, 10000)));
    }

    return new Response(json_encode($rows), 200, array('Content-Type' => 'application/json'));
});

// Test 4: Fortunes
$app->get('/fortunes', function() {
    $db = new mysqli('172.16.98.120', 'benchmarkdbuser', 'benchmarkdbpass', 'hello_world');
    $result = mysqli_query($db, 'SELECT * FROM Fortune');
    while ($row = mysqli_fetch_row($result)) {
        $fortunes[$row[0]] = htmlspecialchars($row[1], ENT_IGNORE);
    }
    $fortunes[] = 'Additional fortune added at request time.';

    asort($fortunes);

    foreach ($fortunes as $i => $fortune) {
        $templates[$i] = '<tr><td>'.$i.'</td><td>'.$fortune.'</td></tr>';
    }

    return new Response('<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'.implode('', $templates).'</table></body></html>');
});

// Test 5: Database updates
$app->get('/updates', function(Request $request) {
    $queries = max(1, min($request->query->get('queries'), 500));
    $db = new mysqli('172.16.98.120', 'benchmarkdbuser', 'benchmarkdbpass', 'hello_world');
    for ($i=0; $i<$queries; $i++) {
        $rows[] = mysqli_fetch_assoc(mysqli_query($db, 'SELECT id, randomNumber FROM World WHERE id = '.rand(1, 10000)));
    }

    mysqli_autocommit($db, FALSE);
    foreach ($rows as $i => $row) {
        $rows[$i]['randomNumber'] = rand(1, 10000);
        mysqli_query($db, "UPDATE World SET randomNumber = {$rows[$i]['randomNumber']} WHERE id = {$row['id']}");
    }
    mysqli_commit($db);

    return new Response(json_encode($rows), 200, array('Content-Type' => 'application/json'));
});

// Test 6: Plaintext
$app->get('/plaintext', function() {
    return new Response('Hello World!', 200, array('Content-Type' => 'text/plain'));
});

$app->run();