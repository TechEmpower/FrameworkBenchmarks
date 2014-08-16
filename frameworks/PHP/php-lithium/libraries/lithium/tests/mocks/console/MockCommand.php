<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\console;

/**
 * This is the Mock Command
 *
 */
class MockCommand extends \lithium\console\Command {

	public $case = null;

	public $face = true;

	/**
	 * Mace.
	 *
	 * @var string Describe value of mace.
	 */
	public $mace = 'test';

	public $race;

	/**
	 * Lace.
	 *
	 * @var boolean Describe value of lace.
	 */
	public $lace = true;

	protected $_dontShow = null;

	protected $_classes = array(
		'response' => 'lithium\tests\mocks\console\MockResponse'
	);

	public function testRun() {
		$this->response->testAction = __FUNCTION__;
	}

	public function clear() {}

	public function testReturnNull() {
		return null;
	}

	public function testReturnTrue() {
		return true;
	}

	public function testReturnFalse() {
		return false;
	}

	public function testReturnNegative1() {
		return -1;
	}

	public function testReturn1() {
		return 1;
	}

	public function testReturn3() {
		return 3;
	}

	public function testReturnString() {
		return 'this is a string';
	}

	public function testReturnEmptyArray() {
		return array();
	}

	public function testReturnArray() {
		return array('a' => 'b');
	}
}

?>