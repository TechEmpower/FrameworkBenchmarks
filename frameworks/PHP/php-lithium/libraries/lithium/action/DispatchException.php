<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2012, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\action;

/**
 * This exception covers a range of scenarios that generally revolve around attempting to dispatch
 * to something which cannot handle a request, i.e. a controller which can't be found, objects
 * which aren't callable, or un-routable (private) controller methods.
 */
class DispatchException extends \RuntimeException {

	protected $code = 404;
}

?>