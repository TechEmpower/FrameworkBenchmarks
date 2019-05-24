<?php

// // OR USE CLOSURES (ANONYMOUS FUNCTIONS) DIRECTLY IN THIS FILE
//
// use Delight\Foundation\App;
//
// $app->get('/', function (App $app) {
// 	// do something
// 	// ...
//
// 	// and return a view
// 	echo $app->view('welcome.html.twig', [
// 		'users' => [ 'Alice', 'Bob' ]
// 	]);
// });
//
// $app->get('/greet/:name', function (App $app, $name) {
// 	// do something
// 	// ...
//
// 	// and return a view
// 	echo $app->view('greeting.html.twig', [
// 		'name' => $name,
// 		'greetingId' => $app->input()->get('greetingId', TYPE_INT)
// 	]);
// });
//
// $app->post('/photos/:id/delete', function (App $app, $id) {
// 	// do something
// 	// ...
// });

use Delight\Foundation\App;

$app->get('/plaintext', ['\App\Controller', 'plaintext']);
$app->get('/json', ['\App\Controller', 'json']);
$app->get('/db', ['\App\Controller', 'db']);
$app->get('/fortunes', ['\App\Controller', 'fortunes']);

$app->get('/queries', ['\App\Controller', 'queries']);
$app->get('/queries/', ['\App\Controller', 'queries']);
$app->get('/queries/:queries', ['\App\Controller', 'queries']);

$app->get('/updates', ['\App\Controller', 'updates']);
$app->get('/updates/', ['\App\Controller', 'updates']);
$app->get('/updates/:queries', ['\App\Controller', 'updates']);

$app->get('/phpinfo', function($app) {
    phpinfo();
});

// return an error page for undefined pages
$app->setStatus(404);
echo $app->view('404.html.twig');
