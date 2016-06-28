<?php

use Illuminate\Http\Request;

require_once __DIR__.'/Models/World.php';
require_once __DIR__.'/Models/Fortune.php';

$app->get("plaintext", function() {
    return response("Hello, World!")->header("Content-Type", "text/plain");
});

$app->get("json", function() {
	return response()->json(["message" => "Hello, World!"]);
});

$app->get("db", function() {
	$id = mt_rand(1, 10000);
	$result = World::find($id);
	return response()->json($result);
});

$app->get("queries", function(Request $request) {
	$query_count = $request->input("queries");
	if ($query_count < 1) {
		$query_count = 1;
	}
	if ($query_count > 500) {
		$query_count = 500;
	}

	$worlds = array();

	for ($i = 0; $i < $query_count; $i++) {
		$id = mt_rand(1, 10000);
		$world = World::find($id);
		$worlds[] = $world;
	}

	return response()->json($worlds);
});

$app->get("updates", function(Request $request) {
	$query_count = $request->input("queries");
	if ($query_count < 1) {
		$query_count = 1;
	}
	if ($query_count > 500) {
		$query_count = 500;
	}

	$worlds = array();

	for ($i = 0; $i < $query_count; $i++) {
		$id = mt_rand(1, 10000);
		$world = World::find($id);
		$world->randomNumber = mt_rand(1, 10000);
		$world->save();
		$worlds[] = $world;
	}

	return response()->json($worlds);
});

$app->get("fortune", function() use ($app) {
	$fortunes = Fortune::all()->toArray();
	$new_fortune = array("id" => 0, "message" => "Additional fortune added at request time.");
	$fortunes[] = $new_fortune;
	$fortunes = array_sort($fortunes, function($value) {
		return $value["message"];
	});
	return view("fortune", ["fortunes" => $fortunes]);
});
