<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\source\mongo_db;

class MockResult extends \lithium\data\source\Result {

	protected $_data = array(
		array('_id' => '4c8f86167675abfabdbf0300', 'title' => 'bar'),
		array('_id' => '5c8f86167675abfabdbf0301', 'title' => 'foo'),
		array('_id' => '6c8f86167675abfabdbf0302', 'title' => 'dib')
	);

	protected $_init = false;

	protected $_autoConfig = array('data', 'name');

	protected $_name = '';

	public $query = array();


	public function hasNext() {
		return ($this->_iterator < count($this->_data));
	}

	public function getNext() {
		$this->_fetchFromResource();
		return $this->_current;
	}

	/**
	 * Fetches the result from the resource and caches it.
	 *
	 * @return boolean Return `true` on success or `false` if it is not valid.
	 */
	protected function _fetchFromResource() {
		if ($this->_iterator < count($this->_data)) {
			$result = current($this->_data);
			$this->_key = $this->_iterator;
			$this->_current = $this->_cache[$this->_iterator++] = $result;
			next($this->_data);
			return true;
		}
		return false;
	}

	public function getName() {
		return $this->_name;
	}

	protected function _close() {
	}

	public function fields(array $fields = array()) {
		$this->query[__FUNCTION__] = $fields;
		return $this;
	}

	public function limit($num) {
		$this->query[__FUNCTION__] = $num;
		return $this;
	}

	public function skip($num) {
		$this->query[__FUNCTION__] = $num;
		return $this;
	}

	public function sort(array $fields = array()) {
		$this->query[__FUNCTION__] = $fields;
		return $this;
	}

	public function count() {
		return reset($this->_data);
	}
}

?>