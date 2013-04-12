<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\core;

class MockCallable extends \lithium\core\Object {

	public $construct = array();

	public $call = array();

	public $get = '';

	public static $callStatic = array();

	public function __construct() {
		$this->construct = func_get_args();
	}

	public function __call($method, $params = array()) {
		return $this->call = compact('method', 'params');
	}

	public static function __callStatic($method, $params) {
		return static::$callStatic = compact('method', 'params');
	}

	public function __get($value) {
		return $this->get = $value;
	}
}

?>