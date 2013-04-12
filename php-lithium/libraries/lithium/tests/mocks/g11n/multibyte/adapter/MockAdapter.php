<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\g11n\multibyte\adapter;

class MockAdapter extends \lithium\core\Object {

	public $testStrlenArgs = array();

	public $testStrposArgs = array();

	public $testStrrposArgs = array();

	public $testSubstrArgs = array();

	public static function enabled() {
		return true;
	}

	public function strlen() {
		$this->testStrlenArgs = func_get_args();
	}

	public function strpos() {
		$this->testStrposArgs = func_get_args();
	}

	public function strrpos() {
		$this->testStrrposArgs = func_get_args();
	}

	public function substr() {
		$this->testSubstrArgs = func_get_args();
	}
}

?>