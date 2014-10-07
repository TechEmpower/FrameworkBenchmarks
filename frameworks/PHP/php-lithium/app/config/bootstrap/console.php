<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

use lithium\console\Dispatcher;
use lithium\core\Environment;

/**
 * This filter sets the environment based on the current request. By default, `$request->env`, for
 * example in the command `li3 help --env=production`, is used to determine the environment.
 *
 */
Dispatcher::applyFilter('run', function($self, $params, $chain) {
	Environment::set($params['request']);
	return $chain->next($self, $params, $chain);
});

/**
 * This filter will convert {:heading} to the specified color codes. This is useful for colorizing
 * output and creating different sections.
 *
 */
// Dispatcher::applyFilter('_call', function($self, $params, $chain) {
// 	$params['callable']->response->styles(array(
// 		'heading' => '\033[1;30;46m'
// 	));
// 	return $chain->next($self, $params, $chain);
// });

?>