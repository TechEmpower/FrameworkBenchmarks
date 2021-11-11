<?php

$router->get('/json', 'Controller@json');
$router->get('/db', 'Controller@db');
$router->get('/queries[/{queries}]', 'Controller@queries');
$router->get('/fortunes', 'Controller@fortunes');
$router->get('/updates[/{queries}]', 'Controller@updates');
$router->get('/plaintext', 'Controller@plaintext');
