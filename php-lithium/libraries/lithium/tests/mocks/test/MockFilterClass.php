<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\test;

class MockFilterClass extends \lithium\core\Object{
	public function __construct($all = false) {
		if ($all) {
			return true;
		}

		return false;
	}

	public function testFunction() {
		$test = true;

		return $test;
	}
}

?>