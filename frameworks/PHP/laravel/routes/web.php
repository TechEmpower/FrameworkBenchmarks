<?php

Route::get('/json', 'Controller@json');
Route::get('/db', 'Controller@db');
Route::get('/queries/{queries?}', 'Controller@queries');
Route::get('/fortunes', 'Controller@fortunes');
Route::get('/updates/{queries?}', 'Controller@updates');
Route::get('/plaintext', 'Controller@plaintext');
