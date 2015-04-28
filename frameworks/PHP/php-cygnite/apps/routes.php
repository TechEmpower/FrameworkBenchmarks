<?php
use Cygnite\Foundation\Application;

if (!defined('CF_SYSTEM')) {
    exit('No External script access allowed');
}

$app = Application::instance();

// Before Router Middle Ware
$app->router->before('GET', '/{:all}', function ()
{
   //echo "This site is under maintenance.";exit;
});

/*
// Dynamic route: /hello/cygnite/3222
$app->router->get('/hello/{:name}/{:digit}', function ($router, $name, $id)
{
   //Router::call('Home.welcome', array($name, $id));
});
*/

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
