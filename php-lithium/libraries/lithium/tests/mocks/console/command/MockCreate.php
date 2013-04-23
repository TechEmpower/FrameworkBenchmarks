<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\console\command;

class MockCreate extends \lithium\console\command\Create {

	protected $_classes = array(
		'response' => 'lithium\tests\mocks\console\MockResponse'
	);

	public function save($template, $params = array()) {
		return $this->_save($template, $params);
	}
}

?>