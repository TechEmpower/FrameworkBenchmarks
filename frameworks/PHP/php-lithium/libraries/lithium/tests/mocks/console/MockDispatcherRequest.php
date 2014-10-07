<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\console;

class MockDispatcherRequest extends \lithium\console\Request {

	public $params = array(
		'command' => '\lithium\tests\mocks\console\MockDispatcherCommand'
	);
}

?>