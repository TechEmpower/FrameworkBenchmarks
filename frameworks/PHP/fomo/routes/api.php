<?php

/** @var Fomo\Router\Router $router */

use Fomo\Database\DB;
use Fomo\Request\Request;

$router->get('/plaintext' , function () {
    return response()->withHeaders([
		'Server' => 'Fomo',
		'Date' => date('D, d M Y H:i:s T'),
	])->plainText('Hello, World!');
});

$router->get('/json' , function () {
    return response()->withHeaders([
		'Server' => 'Fomo',
		'Date' => date('D, d M Y H:i:s T'),
	])->json(['message' => 'Hello, World!']);
});

$router->get('/db' , function () {
	$id = mt_rand(1, 10000);
	// need to pull back a single record from the World table by an $id.
	$world = (array) DB::table('World')->where('id', '=', $id)->get()->toArray()[0];
	return response()->withHeaders([
		'Server' => 'Fomo',
		'Date' => date('D, d M Y H:i:s T'),
	])->json($world);
});

$router->get('/fortunes' , function () {
	
	//$table = DB::table('World');
	$fortunes = DB::table('Fortune')->get()->toArray();

	$fortune = new \stdClass();
	$fortune->id = 0;
	$fortune->message = 'Additional fortune added at request time.';
	array_unshift($fortunes, $fortune);

	// sort the fortunes by message
	usort($fortunes, function($a, $b) {
		return $a->message <=> $b->message;
	});

	ob_start();
	include(storagePath('view/fortunes.php'));
	$html = ob_get_clean();

	return response()->withHeaders([
		'Server' => 'Fomo',
		'Date' => date('D, d M Y H:i:s T'),
	])->html($html);
});

$router->get('/query' , function () {
	$request = Request::getInstance();
	$queries = $request->get('queries');
    if (is_numeric($queries)) {
        $queries = max(1, min($queries, 500));
    } else {
        $queries = 1;
    }

    $worlds = [];
    for ($i = 0; $i < $queries; ++$i) {
		$random_id = mt_rand(1, 10000);
        $world = (array) DB::table('World')->where('id', '=', $random_id)->get()->toArray()[0];
        $worlds[] = $world;
    }
	return response()->withHeaders([
		'Server' => 'Fomo',
		'Date' => date('D, d M Y H:i:s T'),
	])->json($worlds);
});

$router->get('/update' , function () {
	$request = Request::getInstance();
	$queries = $request->get('queries');
    if (is_numeric($queries)) {
        $queries = max(1, min($queries, 500));
    } else {
        $queries = 1;
    }

    $worlds = [];
    for ($i = 0; $i < $queries; ++$i) {
		$random_id = mt_rand(1, 10000);
		$random_number = mt_rand(1, 10000);
		$world = (array) DB::table('World')->where('id', '=', $random_id)->get()->toArray()[0];
		DB::table('World')->where('id', '=', $world['id'])->update(['randomNumber' => $random_number]);
		$world['randomNumber'] = $random_number;
        $worlds[] = $world;
    }
	return response()->withHeaders([
		'Server' => 'Fomo',
		'Date' => date('D, d M Y H:i:s T'),
	])->json($worlds);
});