<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\security\auth\adapter;

class MockHttp extends \lithium\security\auth\adapter\Http {

	public $headers = array();

	protected function _writeHeader($string) {
		$this->headers[] = $string;
	}
}

?>