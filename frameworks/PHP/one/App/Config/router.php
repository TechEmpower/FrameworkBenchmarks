<?php

/**
 * 路由设置
 */

use One\Http\Router;
use \App\Controllers\IndexController;

Router::get('/json', IndexController::class.'@toJson');
Router::get('/plaintext', IndexController::class.'@plaintext');
Router::get('/db', IndexController::class.'@db');
Router::get('/fortunes', IndexController::class.'@fortunes');
Router::get('/queries', IndexController::class.'@queries');
Router::get('/updates', IndexController::class.'@updates');
Router::get('/cached-worlds', IndexController::class.'@cachedWorlds');



//"json_url": "/json",
//"plaintext_url": "/plaintext",
//"db_url": "/db",
//"fortune_url": "/fortunes",
//"update_url": "/updates/",
//"query_url": "/queries/",
