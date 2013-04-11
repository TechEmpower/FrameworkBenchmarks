<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\net\http;

/**
 * The `ClientMediaException` is thrown when a request body could not be understood or decoded by
 * the server.
 *
 * @see lithium\net\http\Media
 */
class ClientMediaException extends \lithium\net\http\MediaException {

	protected $code = 415;
}

?>