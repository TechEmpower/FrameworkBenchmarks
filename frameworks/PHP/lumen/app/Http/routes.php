<?php

use Illuminate\Http\Request;

function update_world(&$world, $key) {
	$world->randomNumber = mt_rand(1, 10000);
}

function compare_fortunes($f1, $f2) {
	return strcmp ($f1->message, $f2->message);
}

$app->get("plaintext", function() use ($app) {
    return response("Hello, World!")->header("Content-Type", "text/plain; charset=utf-8");
});

$app->get("json", function() use ($app) {
	return response()->json(["message" => "Hello, World!"]);
});

$app->get("db", function() use ($app) {
	$id = mt_rand(1, 10000);
	$results = DB::select('select * from world where id = ?', [$id]);
	return response()->json($results[0]);
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
		$world = DB::select("select * from world where id= ?", [$id]);
		$worlds[] = $world[0];
	}

	return response()->json($worlds);
});

$app->get("updates/{queries}", function($queries) use($app) {
	$query_count = $queries;
	if ($query_count < 1) {
		$query_count = 1;
	}
	if ($query_count > 500) {
		$query_count = 500;
	}

	$worlds = array();

	for ($i = 0; $i < $query_count; $i++) {
		$id = mt_rand(1, 10000);
		$world = DB::select("select * from world where id= ?", [$id]);
		$worlds[] = $world[0];
	}

	array_walk($worlds, "update_world");

	return response()->json($worlds);
});

$app->get("fortune", function() use ($app) {
	$fortunes = DB::select("select * from fortune");
	$new_fortune = new stdClass;
	$new_fortune->id = 0;
	$new_fortune->message = "Additional fortune added at request time.";
	$fortunes[] = $new_fortune;
	usort($fortunes, "compare_fortunes");
	return view("fortune", ["fortunes" => $fortunes]);
});
