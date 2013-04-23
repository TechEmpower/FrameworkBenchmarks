<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\test\cases;

use Exception;

class MockTearDownThrowsException extends \lithium\test\Unit {

	public function testNothing() {
		$this->assert(true);
	}

	public function tearDown() {
		throw new Exception('tearDown throws exception');
	}
}

?>