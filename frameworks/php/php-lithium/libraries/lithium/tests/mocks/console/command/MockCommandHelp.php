<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\console\command;

/**
 * This is a mock class for testing help
 *
 */
class MockCommandHelp extends \lithium\console\Command {

	/**
	 * This is a long param.
	 *
	 * @var string
	 */
	public $long = 'default';

	/**
	 * This is a boolean long param.
	 *
	 * @var boolean
	 */
	public $blong = true;

	/**
	 * This is a short param.
	 *
	 * @var boolean
	 */
	public $s = true;

	/**
	 * Don't show this.
	 *
	 * @var array
	 */
	protected $_classes = array(
		'response' => 'lithium\tests\mocks\console\MockResponse'
	);

	/**
	 * This is the run command so don't show it.
	 *
	 * @return boolean
	 */
	public function run() {
		return true;
	}

	/**
	 * This is a task with required args.
	 *
	 * @param string $arg1
	 * @param string $arg2
	 * @return boolean
	 */
	public function sampleTaskWithRequiredArgs($arg1, $arg2) {
		return true;
	}

	/**
	 * This is a task with optional args.
	 *
	 * @param string $arg1
	 * @param string $arg2
	 * @return boolean
	 */
	public function sampleTaskWithOptionalArgs($arg1 = null, $arg2 = null) {
		return true;
	}

	/**
	 * Don't show in the help
	 *
	 * @return void
	 */
	protected function _sampleHelper() {

	}
}

?>