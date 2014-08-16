<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2011, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\core;

class MockErrorHandler extends \lithium\core\ErrorHandler {

	public static function checks($checks = array()) {
		if ($checks) {
			static::$_checks = $checks;
		}
		return static::$_checks;
	}
}

?>