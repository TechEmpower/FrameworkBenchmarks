<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\console;

use lithium\console\Dispatcher;
use lithium\console\Request;

class DispatcherTest extends \lithium\test\Unit {

	protected $_backup = array();

	public function setUp() {
		$this->_backup['_SERVER'] = $_SERVER;
		$_SERVER['argv'] = array();
	}

	public function tearDown() {
		$_SERVER = $this->_backup['_SERVER'];
	}

	public function testEmptyConfigReturnRules() {
		$result = Dispatcher::config();
		$expected = array('rules' => array(
			'command' => array(array('lithium\util\Inflector', 'camelize')),
			'action' => array(array('lithium\util\Inflector', 'camelize', array(false)))
		));
		$this->assertEqual($expected, $result);
	}

	public function testConfigWithClasses() {
		Dispatcher::config(array(
			'classes' => array('request' => 'lithium\tests\mocks\console\MockDispatcherRequest')
		));
		$expected = 'run';
		$result = Dispatcher::run()->testAction;
		$this->assertEqual($expected, $result);
	}

	public function testRunWithCommand() {
		$response = Dispatcher::run(new Request(array(
			'args' => array('lithium\tests\mocks\console\MockDispatcherCommand')
		)));
		$expected = 'run';
		$result = $response->testAction;
		$this->assertEqual($expected, $result);
	}

	public function testRunWithPassed() {
		$response = Dispatcher::run(new Request(array(
			'args' => array('lithium\tests\mocks\console\MockDispatcherCommand', 'with param')
		)));

		$expected = 'run';
		$result = $response->testAction;
		$this->assertEqual($expected, $result);

		$expected = 'with param';
		$result = $response->testParam;
		$this->assertEqual($expected, $result);
	}

	public function testRunWithAction() {
		$response = Dispatcher::run(new Request(array(
			'args' => array('lithium\tests\mocks\console\MockDispatcherCommand', 'testAction')
		)));
		$expected = 'testAction';
		$result = $response->testAction;
		$this->assertEqual($expected, $result);
	}

	public function testInvalidCommand() {
		$expected = (object) array('status' => "Command `\\this\\command\\is\\fake` not found.\n");
		$result = Dispatcher::run(new Request(array(
			'args' => array(
				'\this\command\is\fake',
				'testAction'
			)
		)));

		$this->assertEqual($expected, $result);
	}

	public function testRunWithCamelizingCommand() {
		$expected = (object) array('status' => "Command `FooBar` not found.\n");
		$result = Dispatcher::run(new Request(array(
			'args' => array(
				'foo-bar'
			)
		)));
		$this->assertEqual($expected, $result);

		$expected = (object) array('status' => "Command `FooBar` not found.\n");
		$result = Dispatcher::run(new Request(array(
			'args' => array('foo_bar')
		)));
		$this->assertEqual($expected, $result);
	}

	public function testRunWithCamelizingAction() {
		$result = Dispatcher::run(new Request(array(
			'args' => array(
				'lithium\tests\mocks\console\command\MockCommandHelp',
				'sample-task-with-optional-args'
			)
		)));
		$this->assertTrue($result);

		$result = Dispatcher::run(new Request(array(
			'args' => array(
				'lithium\tests\mocks\console\command\MockCommandHelp',
				'sample_task_with_optional_args'
			)
		)));
		$this->assertTrue($result);
	}

	public function testEnvironmentIsSet() {
		$expected = 'production';
		$response = Dispatcher::run(new Request(array(
			'args' => array(
				'lithium\tests\mocks\console\MockDispatcherCommand',
				'testEnv', '--env=production'
			)
		)));
		$result = $response->environment;
		$this->assertEqual($expected, $result);
	}
}

?>