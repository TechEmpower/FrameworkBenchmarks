<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\storage\session\strategy;

/**
 * A `MissingSignatureException` may be thrown when reading data from a session-based storage that
 * is expecting an HMAC signature, but none is found..
 */
class MissingSignatureException extends \RuntimeException {

	protected $code = 403;
}

?>