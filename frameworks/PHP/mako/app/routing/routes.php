<?php

use app\controllers\Index;

/** @var \mako\http\routing\Routes $routes */

$routes->get('/plaintext', [Index::class, 'plaintext']);
$routes->get('/json', [Index::class, 'json']);
$routes->get('/db', [Index::class, 'db']);
$routes->get('/queries', [Index::class, 'queries']);
$routes->get('/fortunes', [Index::class, 'fortunes']);
$routes->get('/update', [Index::class, 'update']);
