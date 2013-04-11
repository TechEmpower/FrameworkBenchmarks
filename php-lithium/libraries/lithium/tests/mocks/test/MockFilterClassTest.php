<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\test;

use lithium\tests\mocks\test\MockFilterClass;

class MockFilterClassTest extends \lithium\test\Unit {

	public function testNothing() {
		$coverage = new MockFilterClass();

		$this->assertTrue(true);
	}
}

?>