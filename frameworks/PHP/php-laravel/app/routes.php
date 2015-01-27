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

Route::get('/db', function()
{
    $queries = Input::get('queries', 1);
    $worlds = array();

    for($i = 0; $i < $queries; ++$i) {
        $worlds[] = DB::table('World')->find(mt_rand(1, 10000));
    }

    return Response::json($worlds);
});

Route::get('/fortunes', 'BenchController@fortunes');
