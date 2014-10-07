<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\test;

use lithium\tests\mocks\test\MockIntegrationTest;

class IntegrationTest extends \lithium\test\Unit {

	public function testIntegrationHaltsOnFail() {
		$test = new MockIntegrationTest();

		$expected = 2;
		$report = $test->run();
		$result = count($report);

		$this->assertEqual($expected, $result);
	}
}

?>