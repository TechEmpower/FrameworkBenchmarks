<?php

$router = new Phalcon\Mvc\Router(false);

$router->add('/json', array(
    'controller' => 'bench',
    'action' => 'json',
));

// Handles "/db" as well as "/db?queries={queries}"
$router->add('/db', array(
    'controller' => 'bench',
    'action' => 'db',
));

$router->add('/fortunes', array(
    'controller' => 'bench',
    'action' => 'fortunes',
));

$router->add('/update', array(
    'controller' => 'bench',
    'action' => 'update',
));

// Handles "/db" as well as "/db?queries={queries}"
$router->add('/mongodb/db', array(
    'controller' => 'mongobench',
    'action' => 'db',
));

$router->add('/mongodb/fortunes', array(
    'controller' => 'mongobench',
    'action' => 'fortunes',
));

return $router;
