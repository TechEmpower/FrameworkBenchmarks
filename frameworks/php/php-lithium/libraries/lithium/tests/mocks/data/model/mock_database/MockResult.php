<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\model\mock_database;

class MockResult extends \lithium\data\source\Result {

	protected $_records = array();

	protected $_autoConfig = array('resource', 'records');

	/**
	 * Fetches the result from the resource and caches it.
	 *
	 * @return boolean Return `true` on success or `false` if it is not valid.
	 */
	protected function _fetchFromResource() {
		if ($this->_iterator < count($this->_records)) {
			$result = current($this->_records);
			$this->_key = $this->_iterator;
			$this->_current = $this->_cache[$this->_iterator++] = $result;
			next($this->_records);
			return true;
		}
		return false;
	}

	protected function _close() {
	}
}

?>