<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\test;

class MockIntegrationTest extends \lithium\test\Integration {

	public function testPass() {
		$this->assertTrue(true);
	}

	public function testFail() {
		$this->assertTrue(false);
	}

	public function testAnotherPass() {
		$this->assertTrue(true);
	}
}

?>