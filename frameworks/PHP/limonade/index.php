<?php
require_once "vendor/sofadesign/limonade/lib/limonade.php";
require_once "vendor/php-activerecord/php-activerecord/ActiveRecord.php";

function configure() {
	$cfg = ActiveRecord\Config::instance();
	$cfg->set_model_directory("models");
	$cfg->set_connections(array(
		"development" => "mysql://benchmarkdbuser:benchmarkdbpass@localhost/hello_world?charset=utf8"));

option("bas_url", "/");
}

dispatch("/plaintext", "plaintext");
function plaintext() {
	header('Content-Type: text/plain; charset=utf-8');
	return txt("Hello, World!");
}

dispatch("/json", "jsonHandler");
function jsonHandler() {
	header('Content-Type: application/json; charset=utf-8');
	$arr = array("message" => "Hello, World!");
	return json($arr);
}

dispatch("/db", "db");
function db() {
	$id = mt_rand(1, 10000);
	$test = World::find($id);
	return json($test->to_array());
}

dispatch("/queries/:queries", "queries");
function queries() {
	header('Content-Type: application/json; charset=utf-8');
	$query_count = params("queries");
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
		$worlds[] = $world->to_array();
	}

	return json($worlds);

}

dispatch("/updates/:queries", "updates");
function updates() {
	header('Content-Type: application/json; charset=utf-8');
	$query_count = params("queries");
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
                $world->randomnumber = mt_rand(1, 10000);
		$world->save();
		$worlds[] = $world->to_array();
	}

	return json($worlds);
}

function fortune_to_array(&$fortune, $key) {
	$fortune->message = htmlspecialchars($fortune->message);
	$fortune = $fortune->to_array();
}

function compare_fortunes($f1, $f2) {
	return strcmp($f1["message"], $f2["message"]);
}

dispatch("/fortune", "fortune");
function fortune() {
	header('Content-Type: text/html; charset=utf-8');
	$fortunes = Fortune::all();
	array_walk($fortunes, "fortune_to_array");
	$fortunes[] = array("id"=>0, "message"=> "Additional fortune added at request time.");
	usort($fortunes, "compare_fortunes");
	return render("fortune.php", null, array("fortunes" => $fortunes));
}

run();
?>
