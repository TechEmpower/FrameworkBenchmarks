<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\collection;

class MockRecordSet extends \lithium\data\collection\RecordSet {

	public function close() {
		$this->_closed = true;
	}
	/**
	 * Convenience method for lazy loading testing
	 * Reset the `RecordSet` to its inital state after `_construct`
	 *
	 */
	public function reset() {
		if (is_object($this->_result) && method_exists($this->_result, 'rewind')) {
			$this->_closed = false;
			$this->_init = false;
			$this->_started = false;
			$this->_valid = false;
			$this->_data = array();
			$this->_index = array();
			$this->_result->rewind();
			$this->_columns = $this->_columnMap();
			return true;
		}
		return false;
	}

	public function get($var) {
		return $this->{$var};
	}
}

?>