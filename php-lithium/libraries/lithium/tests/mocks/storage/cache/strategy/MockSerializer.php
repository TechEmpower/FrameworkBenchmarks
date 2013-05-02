<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\storage\cache\strategy;

/**
 * Mock strategy.
 * Simulates the 'Serializer' strategy.
 */
class MockSerializer extends \lithium\core\Object {

	/**
	 * Write strategy method.
	 * Serializes the passed data.
	 *
	 * @link http://php.net/manual/en/function.serialize.php PHP Manual: serialize()
	 * @param mixed $data The data to be serialized.
	 * @return string Serialized data.
	 */
	public function write($data) {
		return serialize($data);
	}

	/**
	 * Read strategy method.
	 * Unserializes the passed data.
	 *
	 * @link http://php.net/manual/en/function.unserialize.php PHP Manual: unserialize()
	 * @param string $data Serialized data.
	 * @return mixed Result of unserialization.
	 */
	public function read($data) {
		return unserialize($data);
	}
}

?>