<?php
require_once 'vendor/autoload.php';

error_reporting(-1);

Flight::register('db', PDO::class, [ 'mysql:host=tfb-database;port=3306;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', [ \PDO::ATTR_PERSISTENT => TRUE ] ]);

// JSON test
Flight::route('GET /json', function() {
	Flight::json(['message' => 'Hello, World!']);
});

// Plaintext test
Flight::route('GET /plaintext', function() {
	Flight::response()
		->header('Content-Type', 'text/plain')
		->write('Hello, World!')
		->send();
});

// DB test
Flight::route('GET /db', function() {
	$id = mt_rand(1, 10000);
	$db = Flight::db();
	$stmt = $db->prepare('SELECT * FROM World WHERE id = ?');
    $stmt->execute([ $id ]);
    $world = $stmt->fetch();
    // Cast fields to int so they don't get wrapped with quotes
	$world = [
		'id' => (int) $world['id'],
		'randomNumber' => (int) $world['randomNumber']
	];
	Flight::json($world);
});

// DB multiple test
Flight::route('GET /db-multiple', function () {
	$queries = Flight::request()->query['queries'];
    if (is_numeric($queries)) {
        $queries = max(1, min($queries, 500));
    } else {
        $queries = 1;
    }

	$db = Flight::db();
	$stmt = $db->prepare('SELECT * FROM World WHERE id = ?');
    $worlds = [];
    for ($i = 0; $i < $queries; ++$i) {
		$random_id = mt_rand(1, 10000);
        $stmt->execute([ $random_id ]);
        $world = $stmt->fetch();
        // Cast fields to int so they don't get wrapped with quotes
        $world = [
			'id' => (int) $world['id'],
			'randomNumber' => (int) $world['randomNumber']
		];
        $worlds[] = $world;
    }
    Flight::json($worlds);
});

// DB Update Test
Flight::route('GET /updates', function () {
    $queries = Flight::request()->query['queries'];
    if (is_numeric($queries)) {
        $queries = max(1, min($queries, 500));
    } else {
        $queries = 1;
    }

	$db = Flight::db();

    $select_stmt = $db->prepare('SELECT id FROM World WHERE id = ?');
    $update_stmt = $db->prepare('UPDATE World SET randomNumber = ? WHERE id = ?');

    $worlds = [];
    for ($i = 0; $i < $queries; ++$i) {
        $id = mt_rand(1, 10000);
        $random_number = mt_rand(1, 10000);
        $select_stmt->execute([$id]);
        $world = $select_stmt->fetch();
		$world = [
			'id' => (int) $world['id'],
			'randomNumber' => $random_number
		];

        $update_stmt->execute([ $random_number, (int) $world['id'] ]);

        $worlds[] = $world;
    }

	Flight::json($worlds);
});

// Fortunes Test
Flight::route('GET /fortunes', function() {
	$db = Flight::db();
    $fortunes = $db->query('SELECT * FROM Fortune')->fetchAll(PDO::FETCH_KEY_PAIR);

    $fortunes[0] = 'Additional fortune added at request time.';
    asort($fortunes);

    Flight::render('fortunes.php', [ 'fortunes' => $fortunes ]);
});

Flight::start();