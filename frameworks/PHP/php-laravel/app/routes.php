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

Route::get('/', function()
{
	return View::make('hello');
});

// Route::controller(Controller::detect());

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

// /*
// |--------------------------------------------------------------------------
// | Application 404 & 500 Error Handlers
// |--------------------------------------------------------------------------
// |
// | To centralize and simplify 404 handling, Laravel uses an awesome event
// | system to retrieve the response. Feel free to modify this function to
// | your tastes and the needs of your application.
// |
// | Similarly, we use an event to handle the display of 500 level errors
// | within the application. These errors are fired when there is an
// | uncaught exception thrown in the application. The exception object
// | that is captured during execution is then passed to the 500 listener.
// |
// */

// Event::listen('404', function()
// {
// 	return Response::error('404');
// });

// Event::listen('500', function($exception)
// {
// 	return Response::error('500');
// });

// /*
// |--------------------------------------------------------------------------
// | Route Filters
// |--------------------------------------------------------------------------
// |
// | Filters provide a convenient method for attaching functionality to your
// | routes. The built-in before and after filters are called before and
// | after every request to your application, and you may even create
// | other filters that can be attached to individual routes.
// |
// | Let's walk through an example...
// |
// | First, define a filter:
// |
// |		Route::filter('filter', function()
// |		{
// |			return 'Filtered!';
// |		});
// |
// | Next, attach the filter to a route:
// |
// |		Route::get('/', array('before' => 'filter', function()
// |		{
// |			return 'Hello World!';
// |		}));
// |
// */

// Route::filter('before', function()
// {
// 	// Do stuff before every request to your application...
// });

// Route::filter('after', function($response)
// {
// 	// Do stuff after every request to your application...
// });

// Route::filter('csrf', function()
// {
// 	if (Request::forged()) return Response::error('500');
// });

// Route::filter('auth', function()
// {
// 	if (Auth::guest()) return Redirect::to('login');
// });

