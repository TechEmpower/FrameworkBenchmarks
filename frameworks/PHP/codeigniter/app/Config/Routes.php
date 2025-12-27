<?php

use CodeIgniter\Router\RouteCollection;

/**
 * @var RouteCollection $routes
 */
$routes->get('plaintext', 'Bench::plaintext');
$routes->get('json', 'Bench::json');
$routes->get('fortunes', 'Bench::fortunes');
$routes->get('db', 'Bench::db');
$routes->get('queries/(:alphanum)', 'Bench::queries/$1');
$routes->get('queries/', 'Bench::queries/1');
$routes->get('update/(:alphanum)', 'Bench::update/$1');
$routes->get('update/', 'Bench::update/1');
