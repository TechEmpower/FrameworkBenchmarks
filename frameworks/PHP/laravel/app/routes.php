<?php

/*
|--------------------------------------------------------------------------
| Application Routes
|--------------------------------------------------------------------------
|
| Here is where you can register all of the routes for an application.
| It's a breeze. Simply tell Laravel the URIs it should respond to
| and give it the Closure to execute when that URI is requested.
|
*/

Route::get('/json', function()
{
    return Response::json(array('message' => 'Hello, World!'));
});

Route::get('/plaintext', function()
{
    return "Hello, World!";
});

Route::get('/query', function()
{
    $queries = Input::get('queries', 1);

    if (!is_numeric($queries) || $queries <= 1) {
    	$queries = 1;
    }
    else if ($queries > 500) {
        $queries = 500;
    }

    $worlds = array();

    for($i = 0; $i < $queries; $i++) {
        $worlds[] = DB::table('World')->find(mt_rand(1, 10000));
    }
    return Response::json($worlds);
});

Route::get('/db', function()
{
    return Response::json(DB::table('World')->find(mt_rand(1, 10000)));
});

Route::get('/updates', function()
{
    $queries = Input::get('queries', 1);

    if (!is_numeric($queries) || $queries <= 1) {
    	$queries = 1;
    }
    else if ($queries > 500) {
        $queries = 500;
    }

    $worlds = array();

    for($i = 0; $i < $queries; $i++) {
        $id = mt_rand(1, 10000);
        $random_number = mt_rand(1, 10000);
        $world = DB::table('World')->find($id);
        $world->randomNumber = $random_number;
        DB::table('World')->where('id', $id)->update(['randomNumber' => $random_number]);
        $worlds[] = $world;
    }
    return Response::json($worlds);
});

Route::get('/fortunes', 'BenchController@fortunes');
