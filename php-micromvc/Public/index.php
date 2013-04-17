<?php
/**
 * Index
 *
 * This file defines the basic processing logic flow for the system
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */

// Include bootstrap
require('../Bootstrap.php');

try
{
	// Anything else before we start?
	event('system.startup');

	// Load controller dispatch passing URL routes
	$dispatch = new \Micro\Dispatch(config('Route')->routes);

	// Run controller based on URL path and HTTP request method
	$controller = $dispatch->controller(PATH, getenv('REQUEST_METHOD'));

	// Send the controller response
	$controller->send();

	// One last chance to do something
	event('system.shutdown', $controller);
}
catch (Exception $e)
{
	\Micro\Error::exception($e);
}

