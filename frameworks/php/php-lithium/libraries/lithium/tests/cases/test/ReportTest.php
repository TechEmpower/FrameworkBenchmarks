<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\test;

use lithium\test\Report;
use lithium\test\Group;

class ReportTest extends \lithium\test\Unit {

	public function testInit() {
		$report = new Report(array(
			'title' => 'lithium\tests\mocks\test\MockUnitTest',
			'group' => new Group(array('data' => array('lithium\tests\mocks\test\MockUnitTest')))
		));
		$report->run();

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

	public function testFilters() {
		$report = new Report(array(
			'title' => 'lithium\tests\mocks\test\MockFilterClassTest',
			'group' => new Group(
				array('data' => array('lithium\tests\mocks\test\MockFilterClassTest'))
			),
			'filters' => array("Complexity" => ""),
			'format' => 'html'
		));

		$expected = array('lithium\test\filter\Complexity' => array(
			'name' => 'complexity', 'apply' => array(), 'analyze' => array()
		));
		$result = $report->filters();
		$this->assertEqual($expected, $result);
	}

	public function testStats() {
		$report = new Report(array(
			'title' => 'lithium\tests\mocks\test\MockUnitTest',
			'group' => new Group(array('data' => array('lithium\tests\mocks\test\MockUnitTest')))
		));
		$report->run();

		$expected = 2;
		$result = $report->stats();
		$this->assertEqual($expected, $result['count']['asserts']);
		$this->assertEqual($expected, $result['count']['passes']);
		$this->assertTrue($result['success']);
	}

	public function testRender() {
		$report = new Report(array(
			'title' => '\lithium\tests\mocks\test\MockUnitTest',
			'group' => new Group(array('data' => array('\lithium\tests\mocks\test\MockUnitTest'))),
			'format' => 'txt'
		));
		$report->run();

		$result = $report->render('result', $report->stats());
		$this->assertPattern('#2.*2.*passes.*0.*fails.*0.*exceptions#s', $result);
	}

	public function testSingleFilter() {
		$report = new Report(array(
			'title' => 'lithium\tests\mocks\test\MockFilterClassTest',
			'group' => new Group(array(
				'data' => array('lithium\tests\mocks\test\MockFilterClassTest')
			)),
			'filters' => array("Complexity" => "")
		));
		$report->run();

		$class = 'lithium\test\filter\Complexity';
		$result = $report->results['filters'][$class];
		$this->assertTrue(isset($report->results['filters'][$class]));
	}
}

?>