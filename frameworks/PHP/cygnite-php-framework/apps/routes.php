<?php
use Cygnite\Foundation\Application;
use Cygnite\Base\Router\Router;

if (!defined('CF_SYSTEM')) {
    exit('No External script access allowed');
}

$app = Application::instance();

// Before Router Middle Ware
/*$app->router->before('GET', '/{:all}', function ()
{
   //echo "This site is under maintenance.";exit;
});*/

$app->router->get('/json', function ()
{
    header('Content-type: application/json');
    echo json_encode(array('message'=>'Hello, World!'));
});

$app->router->get('/plaintext', function ()
{
    header("Content-Type: text/plain;");
    echo 'Hello, World!';
});

// Dynamic route: /fortunes
/*$app->router->get('/fortunes', function ()
{
    Router::call('Bench.fortunes', array());
});*/

/*
GET       - resource/           user.getIndex
GET       - resource/new        user.getNew
POST      - resource/           user.postCreate
GET       - resource/{id}       user.getShow
GET       - resource/{id}/edit  user.getEdit
PUT|PATCH - resource/{id}       user.putUpdate
DELETE    - resource/{id}       user.delete
*/
//$app->router->resource('resource', 'user'); // respond to resource routing

/**
 * After routing callback
 * Will call after executing all user defined routing.
 */
$app->router->after(function()
{
   //echo "After Routing callback";
});


$app->router->run();
