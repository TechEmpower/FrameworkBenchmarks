<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\net\http;

/**
 * The `MediaException` is thrown when a request is made to render content in a format not
 * supported.
 *
 * @see lithium\net\http\Media
 */
class MediaException extends \RuntimeException {

	protected $code = 406;
}

?>