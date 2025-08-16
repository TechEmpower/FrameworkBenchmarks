<?php

declare(strict_types=1);

use Hypervel\Support\Facades\Route;

Route::get('/', function () {
    return view('welcome');
});
