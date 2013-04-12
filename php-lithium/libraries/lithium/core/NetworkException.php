<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2012, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\core;

/**
 * A `NetworkException` may be thrown whenever an unsuccessful attempt is made to connect to a
 * remote service over the network. This may be a web service, a database, or another network
 * resource.
 */
class NetworkException extends \RuntimeException {

	protected $code = 503;
}

?>