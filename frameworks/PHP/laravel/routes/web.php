<?php

use App\Http\Controllers\Controller;
use Illuminate\Support\Facades\Route;

Route::get('/json', [Controller::class, 'json']);
Route::get('/db', [Controller::class, 'db']);
Route::get('/queries/{queries?}', [Controller::class, 'queries']);
Route::get('/fortunes', [Controller::class, 'fortunes']);
Route::get('/updates/{queries?}', [Controller::class, 'updates']);
Route::get('/plaintext', [Controller::class, 'plaintext']);

