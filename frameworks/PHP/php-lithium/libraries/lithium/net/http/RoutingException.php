<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\net\http;

/**
 * A `RoutingException` is thrown whenever a the `Router` cannot match a set of parameters against
 * the available collection of attached routes.
 */
class RoutingException extends \RuntimeException {

	protected $code = 500;
}

?>