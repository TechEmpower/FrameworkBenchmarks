<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\core;

class MockExposed extends \lithium\core\Object {

	protected $_internal = 'secret';

	public function tamper() {
		$internal =& $this->_internal;

		return $this->_filter(__METHOD__, array(), function() use (&$internal) {
			$internal = 'tampered';
			return true;
		});
	}

	public function get() {
		return $this->_internal;
	}
}

?>