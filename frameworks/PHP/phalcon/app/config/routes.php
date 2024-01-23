<?php

use Phalcon\Mvc\Router;

$router = new Router(false);

$router->addGet('/json', [
    'controller' => 'bench',
    'action' => 'json',
]);

$router->addGet('/db', [
    'controller' => 'bench',
    'action' => 'db',
]);

$router->addGet('/queries', [
    'controller' => 'bench',
    'action' => 'queries',
]);

$router->addGet('/fortunes', [
    'controller' => 'bench',
    'action' => 'fortunes',
]);

$router->addGet('/update', [
    'controller' => 'bench',
    'action' => 'update',
]);

$router->addGet('/plaintext', [
    'controller' => 'bench',
    'action' => 'plaintext',
]);

$router->addGet('/mongodb/db', [
    'controller' => 'mongo',
    'action' => 'db',
]);

$router->addGet('/mongodb/queries', [
    'controller' => 'mongo',
    'action' => 'queries',
]);

$router->addGet('/mongodb/fortunes', [
    'controller' => 'mongo',
    'action' => 'fortunes',
]);

return $router;
