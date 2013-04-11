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
 * For testing strategies that need construct-time parameters.
 */
class MockConfigurizer extends \lithium\core\Object {

	public static $parameters = array();

	/**
	 * Constructor.
	 * @param array $config
	 */
	public function __construct(array $config = array()) {
		static::$parameters = $config;
	}

	/**
	 * Write strategy method.
	 *
	 * @param mixed $data The data to be modified.
	 * @return string Modified data.
	 */
	public static function write($data) {
		return static::$parameters;
	}

	/**
	 * Read strategy method.
	 * Unserializes the passed data.
	 *
	 * @param string $data Data read.
	 * @return mixed Modified data.
	 */
	public static function read($data) {
		return static::$parameters;
	}
}

?>