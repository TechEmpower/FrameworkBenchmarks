<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\template\helper;

use lithium\action\Request;

class MockFormRenderer extends \lithium\template\view\Renderer {

	public function request() {
		if (empty($this->_request)) {
			$this->_request = new Request();
			$this->_request->params += array('controller' => 'posts', 'action' => 'add');
		}
		return $this->_request;
	}

	public function render($template, $data = array(), array $options = array()) {
	}
}

?>