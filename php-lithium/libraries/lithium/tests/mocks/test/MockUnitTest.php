<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\test;

class MockUnitTest extends \lithium\test\Unit {

	public function testNothing() {
		$this->assertTrue(true);
	}

	/**
	 * This method is used in a test and is *line sensitive*. The corresponding
	 * test's expectations needs to be adapted if the line of the `assert()`
	 * call changes.
	 *
	 * @see lithium\tests\cases\test\UnitTest::testAssertBacktraces()
	 */
	public function testSomething() {
		$this->assert(true);
	}

	/**
	 * This method is used in a test to prepare it.
	 *
	 * @see lithium\tests\cases\test\UnitTest::testExpectExceptionNotThrown()
	 */
	public function prepareTestExpectExceptionNotThrown() {
		$this->expectException('test');
	}

	public function compare($type, $expected, $result = null, $trace = null) {
		return parent::_compare($type, $expected, $result, $trace);
	}

	public function handleException($exception, $lineFlag = null) {
		return parent::_handleException($exception, $lineFlag);
	}

	public function expected() {
		return $this->_expected;
	}
}

?>