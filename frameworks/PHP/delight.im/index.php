<?php

/*
 * PHP-Foundation (https://github.com/delight-im/PHP-Foundation)
 * Copyright (c) delight.im (https://www.delight.im/)
 * Licensed under the MIT License (https://opensource.org/licenses/MIT)
 */

// define the path to the Composer autoloader
$autoLoadPath = __DIR__ . '/vendor/autoload.php';

// if the Composer autoloader has been generated
if (@\is_file($autoLoadPath)) {
	// include the autoloader that loads all dependencies
	require $autoLoadPath;
}
// if the Composer autoloader is missing on the file system
else {
	// explain how the autoloader file can be generated
	echo 'Get Composer (https://getcomposer.org/) and run \'composer install\' in your local version of the directory \''.basename(__DIR__).'\'';
	exit;
}

if (@\is_dir(__DIR__ . '/config')) {
	// load the configuration into the environment variables
	$envLoader = new \Dotenv\Dotenv(__DIR__ . '/config');
	$envLoader->overload();
}

// in debug mode
if (!isset($_ENV['APP_DEBUG']) || $_ENV['APP_DEBUG'] === '1') {
	// enable assertions
	\ini_set('assert.active', 1);
	@\ini_set('zend.assertions', 1);
	\ini_set('assert.exception', 1);

	// show errors
	\ini_set('display_errors', 1);
	\ini_set('display_startup_errors', 1);

	// terminate on errors and warnings
	(new \Whoops\Run())->pushHandler(new \Whoops\Handler\PlainTextHandler())->register();
}
// in production mode
else {
	// disable assertions
	\ini_set('assert.active', 0);
	@\ini_set('zend.assertions', -1);
	\ini_set('assert.exception', 0);

	// hide errors
	\ini_set('display_errors', 0);
	\ini_set('display_startup_errors', 0);
}

if (!isset($_ENV['APP_DEBUG']) && !isset($_ENV['APP_PUBLIC_URL'])) {
	throw new \RuntimeException('Environment variables not set');
}

// if the internal character encoding has been configured
if (isset($_ENV['APP_CHARSET'])) {
	// set the internal charset for PHP
	\mb_internal_encoding($_ENV['APP_CHARSET']);
}

// if the default timezone has been configured
if (isset($_ENV['APP_DEFAULT_TIMEZONE'])) {
	// set the timezone for PHP
	\date_default_timezone_set($_ENV['APP_DEFAULT_TIMEZONE']);
}

// define a shorthand for access to the string handling methods
function s($str) {
	return new \Delight\Str\Str($str);
}

// define a shorthand for access to HTML escaping
function e($str) {
	return s($str)->escapeForHtml();
}

// create the main application instance
$app = new \Delight\Foundation\App(__DIR__ . '/storage/app', __DIR__ . '/views', __DIR__ . '/storage/framework');

// set the default content type and the correct charset for HTTP responses
$app->setContentType('html');

// reluctantly put some constants into the global namespace for maximum convenience
\define('TYPE_STRING', \Delight\Foundation\Input::DATA_TYPE_STRING);
\define('TYPE_INT', \Delight\Foundation\Input::DATA_TYPE_INT);
\define('TYPE_BOOL', \Delight\Foundation\Input::DATA_TYPE_BOOL);
\define('TYPE_FLOAT', \Delight\Foundation\Input::DATA_TYPE_FLOAT);
\define('TYPE_EMAIL', \Delight\Foundation\Input::DATA_TYPE_EMAIL);
\define('TYPE_URL', \Delight\Foundation\Input::DATA_TYPE_URL);
\define('TYPE_IP', \Delight\Foundation\Input::DATA_TYPE_IP);
\define('TYPE_TEXT', \Delight\Foundation\Input::DATA_TYPE_TEXT);
\define('TYPE_RAW', \Delight\Foundation\Input::DATA_TYPE_RAW);

// include the actual application code
require __DIR__ . '/app/index.php';
