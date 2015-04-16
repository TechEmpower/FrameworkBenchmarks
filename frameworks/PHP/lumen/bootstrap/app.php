<?php
require_once __DIR__.'/../vendor/autoload.php';

Dotenv::load(__DIR__.'/../');

$app = new Laravel\Lumen\Application;
$app->withFacades();

$app->singleton(
    'Illuminate\Contracts\Debug\ExceptionHandler',
    'App\Exceptions\Handler'
);

$app->singleton(
    'Illuminate\Contracts\Console\Kernel',
    'App\Console\Kernel'
);

require __DIR__.'/../app/Http/routes.php';

return $app;

