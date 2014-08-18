<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\storage\cache\strategy;

/**
 * A PHP base64-encoding strategy.
 */
class Base64 extends \lithium\core\Object {

	/**
	 * Write strategy method.
	 *
	 * Base64-encodes the passed data.
	 *
	 * @link http://php.net/manual/en/function.base64-encode.php PHP Manual: base64_encode()
	 * @param mixed $data The data to be serialized.
	 * @return string Serialized data.
	 */
	public function write($data) {
		return base64_encode($data);
	}

	/**
	 * Read strategy method.
	 *
	 * Unserializes the passed data.
	 *
	 * @link http://php.net/manual/en/function.base64-decode.php PHP Manual: base64_decode()
	 * @param string $data Serialized data.
	 * @return mixed Result of unserialization.
	 */
	public function read($data) {
		return base64_decode($data);
	}
}

?>