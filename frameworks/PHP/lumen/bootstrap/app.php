<?php
require_once __DIR__.'/../vendor/autoload.php';

Dotenv::load(__DIR__.'/../');

$app = new Laravel\Lumen\Application;
$app->withFacades();
$app->withEloquent();

require __DIR__.'/../app/Http/routes.php';

return $app;

