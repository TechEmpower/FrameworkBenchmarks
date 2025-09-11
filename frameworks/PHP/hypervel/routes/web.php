<?php

declare(strict_types=1);

use App\Http\Controllers\IndexController;
use Hypervel\Support\Facades\Route;

Route::get('/json', [IndexController::class, 'json']);
Route::get('/db', [IndexController::class, 'db']);
Route::get('/queries[/{queries}]', [IndexController::class, 'queries']);
Route::get('/fortunes', [IndexController::class, 'fortunes']);
Route::get('/updates[/{queries}]', [IndexController::class, 'updates']);
Route::get('/plaintext', [IndexController::class, 'plaintext']);
