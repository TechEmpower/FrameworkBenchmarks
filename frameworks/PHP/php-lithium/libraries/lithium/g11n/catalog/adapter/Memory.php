<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\g11n\catalog\adapter;

/**
 * The `Memory` class is an adapter for reading and writing data during runtime.
 *
 * Written data is stored in memory and lost after the end of the script execution. The
 * adapter is also very useful for testing.
 */
class Memory extends \lithium\g11n\catalog\Adapter {

	/**
	 * Holds data during runtime.
	 *
	 * @var array
	 */
	protected $_data = array();

	/**
	 * Reads data.
	 *
	 * @param string $category A category.
	 * @param string $locale A locale identifier.
	 * @param string $scope The scope for the current operation.
	 * @return array
	 */
	public function read($category, $locale, $scope) {
		$scope = $scope ?: 'default';

		if (isset($this->_data[$scope][$category][$locale])) {
			return $this->_data[$scope][$category][$locale];
		}
	}

	/**
	 * Writes data.
	 *
	 * @param string $category A category.
	 * @param string $locale A locale identifier.
	 * @param string $scope The scope for the current operation.
	 * @param array $data The data to write.
	 * @return boolean
	 */
	public function write($category, $locale, $scope, array $data) {
		$scope = $scope ?: 'default';

		if (!isset($this->_data[$scope][$category][$locale])) {
			$this->_data[$scope][$category][$locale] = array();
		}
		foreach ($data as $item) {
			$this->_data[$scope][$category][$locale] = $this->_merge(
				$this->_data[$scope][$category][$locale],
				$this->_prepareForWrite($item)
			);
		}
		return true;
	}
}

?>