<?php
//Set debugging mode
$debug = true;

//Allowed hosts
$allowed_hosts = [];

//Set templates directory
$templates = [
  '/template'
];

//Global middlewares
$middlewares = [];

//Database settings
$db['db'] = [
    'dsn' => '',
    'hostname' => env('DB_HOST'),
    'port' => env('DB_PORT'),
    'username' => env('DB_USERNAME'),
    'password' => env('DB_PASSWORD'),
    'database' => env('DB_DATABASE'),
    'driver' => 'pdo',
    'char_set' => 'utf8',
];

//Static URL
$static_url = '/';

//Static files DIR
$static_dir = '/static';

//Ignore trailing slashes
$ignore_trailing_slash = true;

//Set default language
$default_language = 'en';

//Set default timezone
date_default_timezone_set('Asia/Kolkata');
