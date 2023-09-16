<?php

use CodeIgniter\Router\RouteCollection;

/**
 * @var RouteCollection $routes
 */

// We get a performance increase by specifying the default
// route since we don't have to scan directories.
//$routes->get('/', 'Home::index');
$routes->get('plaintext', 'Bench::plaintext');
$routes->get('json', 'Bench::json');
$routes->get('fortunes', 'Bench::fortunes'); // /(:num)
$routes->get('db', 'Bench::db');
$routes->get('queries/(:alphanum)', 'Bench::queries/$1');
$routes->get('queries/', 'Bench::queries/1');
$routes->get('update/(:alphanum)', 'Bench::update/$1');
$routes->get('update/', 'Bench::update/1');


