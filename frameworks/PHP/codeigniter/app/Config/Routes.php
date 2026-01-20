<?php
/**
 * @var CodeIgniter\Router\RouteCollection $routes
 */
$routes->get('plaintext', static fn () => service('response')->setContentType('text/plain')->setBody('Hello, World!'));
$routes->get('json', static fn () => service('response')->setJSON(['message' => 'Hello, World!']));

$routes->get('db',                      'Full::db');
$routes->get('queries/(:alphanum)',     'Full::queries/$1');
$routes->get('queries/',                'Full::queries/1');
$routes->get('update/(:alphanum)',      'Full::update/$1');
$routes->get('update/',                 'Full::update/1');
$routes->get('fortunes',                'Full::fortunes');

$routes->get('raw/db',                  'Raw::db');
$routes->get('raw/queries/(:alphanum)', 'Raw::queries/$1');
$routes->get('raw/queries/',            'Raw::queries/1');
$routes->get('raw/update/(:alphanum)',  'Raw::update/$1');
$routes->get('raw/update/',             'Raw::update/1');
$routes->get('raw/fortunes',            'Raw::fortunes');
