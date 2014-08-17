<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\console;

use lithium\console\Router;
use lithium\console\Request;

class RouterTest extends \lithium\test\Unit {

	protected $_backup;

	public function setUp() {
		$this->_backup = $_SERVER;
		$_SERVER['argv'] = array();
	}

	public function tearDown() {
		$_SERVER = $this->_backup;
	}

	public function testParseNoArgumentsNoOptions() {
		$expected = array(
			'command' => null, 'action' => 'run', 'args' => array()
		);
		$result = Router::parse();
		$this->assertEqual($expected, $result);
	}

	public function testParseArguments() {
		$expected = array(
			'command' => 'test', 'action' => 'action',
			'args' => array('param')
		);
		$result = Router::parse(new Request(array(
			'args' => array('test', 'action', 'param')
		)));
		$this->assertEqual($expected, $result);
	}

	public function testParseGnuStyleLongOptions() {
		$expected = array(
			'command' => 'test', 'action' => 'run', 'args' => array(),
			'case' => 'lithium.tests.cases.console.RouterTest'
		);
		$result = Router::parse(new Request(array(
			'args' => array(
				'test', 'run',
				'--case=lithium.tests.cases.console.RouterTest'
			)
		)));
		$this->assertEqual($expected, $result);

		$expected = array(
			'command' => 'test', 'action' => 'run', 'args' => array(),
			'case' => 'lithium.tests.cases.console.RouterTest',
			'phase' => 'drowning'
		);
		$result = Router::parse(new Request(array(
			'args' => array(
				'test',
				'--case=lithium.tests.cases.console.RouterTest',
				'--phase=drowning'
			)
		)));
		$this->assertEqual($expected, $result);
	}

	public function testParseGnuStyleLongOptionsContainingDash() {
		$expected = array(
			'command' => 'test', 'action' => 'run', 'args' => array(),
			'foo-bar' => 'something'
		);
		$result = Router::parse(new Request(array(
			'args' => array(
				'test', 'run',
				'--foo-bar=something'
			)
		)));
		$this->assertEqual($expected, $result);
	}

	public function testParseShortOption() {
		$expected = array(
			'command' => 'test', 'action' => 'action', 'args' => array(),
			'i' => true
		);
		$result = Router::parse(new Request(array(
			'args' => array('test', 'action', '-i')
		)));
		$this->assertEqual($expected, $result);

		$expected = array(
			'command' => 'test', 'action' => 'action', 'args' => array('something'),
			'i' => true
		);
		$result = Router::parse(new Request(array(
			'args' => array('test', 'action', '-i', 'something')
		)));
		$this->assertEqual($expected, $result);
	}

	public function testParseShortOptionAsFirst() {
		$expected = array(
			'command' => 'test', 'action' => 'action', 'args' => array(),
			'i' => true
		);
		$result = Router::parse(new Request(array(
			'args' => array('-i', 'test', 'action')
		)));
		$this->assertEqual($expected, $result);

		$expected = array(
			'command' => 'test', 'action' => 'action', 'args' => array('something'),
			'i' => true
		);
		$result = Router::parse(new Request(array(
			'args' => array('-i', 'test', 'action', 'something')
		)));
		$this->assertEqual($expected, $result);
	}

	public function testParseGnuStyleLongOptionAsFirst() {
		$expected = array(
			'command' => 'test', 'action' => 'action', 'long' => 'something', 'i' => true,
			'args' => array()
		);
		$result = Router::parse(new Request(array(
			'args' => array('--long=something', 'test', 'action', '-i')
		)));
		$this->assertEqual($expected, $result);
	}
}

?>