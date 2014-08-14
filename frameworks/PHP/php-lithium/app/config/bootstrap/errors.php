<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

use lithium\core\ErrorHandler;
use lithium\action\Response;
use lithium\net\http\Media;

ErrorHandler::apply('lithium\action\Dispatcher::run', array(), function($info, $params) {
	$response = new Response(array(
		'request' => $params['request'],
		'status' => $info['exception']->getCode()
	));

	Media::render($response, compact('info', 'params'), array(
		'library' => true,
		'controller' => '_errors',
		'template' => 'development',
		'layout' => 'error',
		'request' => $params['request']
	));
	return $response;
});

?>