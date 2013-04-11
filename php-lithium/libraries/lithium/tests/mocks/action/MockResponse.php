<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\action;

class MockResponse extends \lithium\action\Response {

	public $testHeaders = array();

	public function render() {
		$this->testHeaders = array();
		parent::render();
		$this->headers = array();
	}

	protected function _writeHeader($header, $code = null) {
		$this->testHeaders[] = $header;
	}
}

?>