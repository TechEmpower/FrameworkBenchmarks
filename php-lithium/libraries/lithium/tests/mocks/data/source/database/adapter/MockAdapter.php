<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\source\database\adapter;

class MockAdapter extends \lithium\data\source\Database {

	/**
	 * An array of records to test.
	 *
	 * This is useful for testing how an `Adapter` returns the data when invoking
	 * the `Adapter::result()` function
	 *
	 * @var array
	 */
	protected $_records = array();

	/**
	 * A list of columns for the current test
	 *
	 * @var array
	 */
	protected $_columns = array();

	/**
	 * Holds an array of values that should be processed on initialisation.
	 *
	 * @var array
	 */
	protected $_autoConfig = array('records', 'columns');

	/**
	 * Internal pointer to indicate the current record.
	 *
	 * @var array
	 */
	protected $_pointer = 0;

	public function __construct(array $config = array()) {
		$defaults =  array('records' => array(), 'columns' => array());
		$config['autoConnect'] = false;
		parent::__construct((array) $config + $defaults);
	}

	public function connect() {
		return true;
	}

	public function disconnect() {
		return true;
	}

	public function sources($class = null) {
	}

	public function encoding($encoding = null) {
		return $encoding ?: '';
	}

	public function describe($entity, $fields = array(), array $meta = array()) {
		return $this->_instance('schema', compact('fields', 'meta'));
	}

	public function create($record, array $options = array()) {
		return true;
	}

	public function read($query, array $options = array()) {
		return true;
	}

	public function update($query, array $options = array()) {
		return true;
	}

	public function delete($query, array $options = array()) {
		return true;
	}

	public function result($type, $resource, $context) {
		$return = null;
		if (array_key_exists($this->_pointer, $this->_records)) {
			$return = $this->_records[$this->_pointer++];
		}
		return $return;
	}

	public function error() {
		return false;
	}

	public function name($name) {
		return $name;
	}

	public function value($value, array $schema = array()) {
		if (is_array($value)) {
			return parent::value($value, $schema);
		}
		return $value;
	}

	public function schema($query, $resource = null, $context = null) {
		return $this->_columns;
	}

	public function conditions($conditions, $context, array $options = array()) {
		return $conditions;
	}

	public function fields($fields, $context) {
		if (empty($fields)) {
			return $context->fields();
		}
		return $fields;
	}

	public function limit($limit, $context) {
		if (empty($limit)) {
			return '';
		}
		return $limit;
	}

	public function order($order, $context) {
		if (empty($order)) {
			return '';
		}
		return $order;
	}

	public function renderCommand($type, $data = null, $context = null) {
		return '';
	}

	public function key() {
	}

	protected function _execute($sql) {
		return $sql;
	}

	protected function _insertId($query) {}
}

?>