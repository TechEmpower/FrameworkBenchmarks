<?php

$router = new Phalcon\Mvc\Router();

$router->add('/json', array(
    'controller' => 'bench',
    'action' => 'json',
));

// Handles "/db" as well as "/db?queries={queries}"
$router->add('/db', array(
    'controller' => 'bench',
    'action' => 'db',
));

return $router;