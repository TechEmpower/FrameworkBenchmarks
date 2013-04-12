<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\test;

use lithium\test\Group;
use lithium\test\Report;
use lithium\test\Dispatcher;
use lithium\util\Collection;
use lithium\tests\mocks\test\cases\MockTest;
use lithium\tests\mocks\test\cases\MockTestErrorHandling;
use lithium\tests\mocks\test\cases\MockSkipThrowsException;
use lithium\tests\mocks\test\cases\MockSetUpThrowsException;
use lithium\tests\mocks\test\cases\MockTearDownThrowsException;

class DispatcherTest extends \lithium\test\Unit {

	public function testRunDefaults() {
		$report = Dispatcher::run();
		$this->assertTrue($report instanceof Report);

		$result = $report->group;
		$this->assertTrue($result instanceof Group);
	}

	public function testRunWithReporter() {
		$report = Dispatcher::run(null, array(
			'reporter' => function($info) {
				return $info;
			}
		));
		$this->assertTrue($report instanceof Report);

		$result = $report->group;
		$this->assertTrue($result instanceof Group);
	}

	public function testRunCaseWithString() {
		$report = Dispatcher::run('lithium\tests\mocks\test\MockUnitTest');

		$expected = 'lithium\tests\mocks\test\MockUnitTest';
		$result = $report->title;
		$this->assertEqual($expected, $result);

		$expected = 'testNothing';
		$result = $report->results['group'][0][0]['method'];
		$this->assertEqual($expected, $result);

		$expected = 'pass';
		$result = $report->results['group'][0][0]['result'];
		$this->assertEqual($expected, $result);
	}

	public function testRunGroupWithString() {
		$report = Dispatcher::run('lithium\tests\mocks\test');

		$expected = 'lithium\tests\mocks\test';
		$result = $report->title;
		$this->assertEqual($expected, $result);

		$expected = new Collection(array('data' => array(
			new MockSetUpThrowsException(),
			new MockSkipThrowsException(),
			new MockTearDownThrowsException(),
			new MockTest(),
			new MockTestErrorHandling()
		)));
		$result = $report->group->tests();
		$this->assertEqual($expected, $result);
		$expected = 'testNothing';
		$result = $report->results['group'][3][0]['method'];
		$this->assertEqual($expected, $result);

		$expected = 'pass';
		$result = $report->results['group'][3][0]['result'];
		$this->assertEqual($expected, $result);
	}
}

?>