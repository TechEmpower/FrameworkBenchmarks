<?php

$pdo = new PDO(
    'pgsql:host=tfb-database;dbname=hello_world',
    'benchmarkdbuser',
    'benchmarkdbpass',
    [
        PDO::ATTR_DEFAULT_FETCH_MODE  => PDO::FETCH_ASSOC,
        PDO::ATTR_ERRMODE             => PDO::ERRMODE_EXCEPTION,
        PDO::ATTR_EMULATE_PREPARES    => false
    ]
);

$randomWorldStatement = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id = ?');
$fortuneStatement = $pdo->query('SELECT id, message FROM Fortune');
$updatesByCount = [];

$handleJson = function () {
    header('Content-Type: application/json');
    echo json_encode(['message' => 'Hello, World!']);
};

$handlePlainText = function () {
    header('Content-Type: text/plain');
    echo 'Hello, World!';
};

$handleDb = function () use ($pdo, $randomWorldStatement) {
    $randomWorldStatement->execute([mt_rand(1, 10000)]);
    header('Content-Type: application/json');
    echo json_encode($randomWorldStatement->fetch(PDO::FETCH_ASSOC), JSON_NUMERIC_CHECK);
};

$handleQuery = function () use ($pdo, $randomWorldStatement) {
    $count = 1;
    if ((int)$_GET['queries'] > 1) {
        $count = min($_GET['queries'], 500);
    }
    while ($count--) {
        $randomWorldStatement->execute([mt_rand(1, 10000)]);
        $arr[] = $randomWorldStatement->fetch(PDO::FETCH_ASSOC);
    }
    header('Content-Type: application/json');
    echo json_encode($arr, JSON_NUMERIC_CHECK);
};

$handleFortunes = function () use ($pdo, $fortuneStatement): void {
    $fortuneStatement->execute();
    $arr = $fortuneStatement->fetchAll(PDO::FETCH_KEY_PAIR);
    $arr[0] = 'Additional fortune added at request time.';
    asort($arr);
    $tableRow = '';
    foreach ($arr as $id => $message) {
        $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
        $tableRow .= "<tr><td>$id</td><td>$message</td></tr>";
    }
    echo "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$tableRow</table></body></html>";
};

// Note: Identical to the implementation in php-ngx
$handleUpdates = function () use ($pdo, $randomWorldStatement, &$updatesByCount) {
    header('Content-Type: application/json');
    $queryCount = 1;
    if ((int)$_GET['queries'] > 1) {
        $queryCount = min($_GET['queries'], 500);
    }

    $worlds = [];
    while ($queryCount--) {
        $randomWorldStatement->execute([mt_rand(1, 10000)]);
        $world = $randomWorldStatement->fetch();
        $world['randomNumber'] = mt_rand(1, 10000);
        $worlds[] = $world;
    }
    $rows = count($worlds);

    if (!isset($updatesByCount[$rows])) {
        $sql = 'UPDATE world SET randomNumber = CASE id'
            . str_repeat(' WHEN ?::INTEGER THEN ?::INTEGER ', $rows)
            . 'END WHERE id IN ('
            . str_repeat('?::INTEGER,', $rows - 1) . '?::INTEGER)';

        $updatesByCount[$rows] = $pdo->prepare($sql);
    }

    $val = [];
    $keys = [];
    foreach ($worlds as $world) {
        $val[] = $keys[] = $world['id'];
        $val[] = $world['randomNumber'];
    }

    $updatesByCount[$rows]->execute([...$val, ...$keys]);
    echo json_encode($worlds, JSON_NUMERIC_CHECK);
};

$dropRequest = function () {
    http_response_code(404);
    echo "<h3>404 Not Found</h3>";
};

$handlers = [
    '/json' => $handleJson,
    '/plaintext' => $handlePlainText,
    '/db' => $handleDb,
    '/query' => $handleQuery,
    '/fortunes' => $handleFortunes,
    '/update' => $handleUpdates
];

$maxRequests = 20_000;
$requestCount = 0;

$handleHttpRequest = function () use ($handlers, $dropRequest, &$requestCount) {
    $path = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
    $handler = $handlers[$path] ?? $dropRequest;
    $handler();
    $requestCount++;
};

while (frankenphp_handle_request($handleHttpRequest) && $requestCount < $maxRequests) {
}

