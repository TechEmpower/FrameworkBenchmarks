<?php
declare(strict_types=1);

use App\ORM;
use Comet\Comet;
use App\Controllers\DbController;
use App\Controllers\QueryController;
use App\Controllers\UpdateController;
use App\Controllers\FortuneController;

require_once __DIR__ . '/vendor/autoload.php';

// TODO Use Timer to setup Date header once per second!

ORM::init();

$app = new Comet([
    'host' => '0.0.0.0',
    'port' => 8080,
    'debug' => false,
    'logger' => null,
]);

// #1 Plaintext
$app->get('/plaintext',
    function ($request, $response) {        
        return $response
            ->with('Hello, World!')
            ->withHeader('Date', gmdate('D, d M Y H:i:s') . ' GMT');
});

// #2 JSON Serialization
$app->get('/json', 
    function ($request, $response) {        
        return $response            
            ->with([ 'message' => 'Hello, World!' ])
            ->withHeader('Date', gmdate('D, d M Y H:i:s') . ' GMT');
});

// #3 Single Database Query
$app->get('/db',    
    DbController::class);

// #4: Multiple Database Queries
$app->get('/query',    
    QueryController::class);

// #5: Update Database Rows
$app->get('/update',    
    UpdateController::class);

// #6 Return Fortunes
$app->get('/fortune',    
    FortuneController::class);

$app->run();
