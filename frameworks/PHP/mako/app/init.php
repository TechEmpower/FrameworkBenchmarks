<?php

/**
 * Configure PHP error reporting.
 * @see http://php.net/manual/en/function.error-reporting.php
 */
error_reporting(E_ALL | E_STRICT);

/*
 * Choose if errors that are NOT caught by the Mako error and exception handlers should be
 * printed to the screen as part of the output or if they should be hidden from the user.
 * It is recommended to set this value to false when you are in production.
 */
ini_set('display_errors', true);

/*
 * Override the default path for error logs.
 */
ini_set('error_log', __DIR__ . '/storage/logs/error_' . gmdate('Y_m_d') . '.log');

/*
 * Convert all errors to ErrorExceptions.
 */
set_error_handler(function($code, $message, $file, $line)
{
	if((error_reporting() & $code) !== 0)
	{
		throw new ErrorException($message, $code, $code, $file, $line);
	}

	return true;
});

/*
 * Define some constants.
 */
define('MAKO_APPLICATION_PATH', __DIR__);

/**
 * Include the composer autoloader.
 */
include dirname(__DIR__) . '/vendor/autoload.php';
