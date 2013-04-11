<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\core;

class MockStaticInstantiator extends \lithium\core\StaticObject {

	protected static $_classes = array('request' => 'lithium\tests\mocks\core\MockRequest');

	public static function instance($name, array $config = array()) {
		return static::_instance($name, $config);
	}

	protected static function _foo() {
		return false;
	}
}

?>