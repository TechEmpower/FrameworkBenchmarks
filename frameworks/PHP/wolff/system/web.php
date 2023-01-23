<?php

use Wolff\Core\Container;
use Wolff\Core\Route;

/**
 * Use this file for declaring routes, middlewares and more...
 */

Route::get('/plaintext', [ Controller\Home::class, 'plaintext' ]);
Route::get('/json', [ Controller\Home::class, 'json' ]);
Route::get('/db', [ Controller\Home::class, 'db' ]);
Route::get('/queries', [ Controller\Home::class, 'queries' ]);
Route::get('/update', [ Controller\Home::class, 'update' ]);
Route::get('/fortunes', [ Controller\Home::class, 'fortunes' ]);

Container::singleton('db', function() {
    return new \Wolff\Core\DB;
});
