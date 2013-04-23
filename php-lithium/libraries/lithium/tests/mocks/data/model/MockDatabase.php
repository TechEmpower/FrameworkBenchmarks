<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\model;

use lithium\tests\mocks\data\model\mock_database\MockResult;

class MockDatabase extends \lithium\data\source\Database {

	/**
	 * Mock column type definitions.
	 *
	 * @var array
	 */
	protected $_columns = array(
		'primary_key' => array('name' => 'NOT NULL AUTO_INCREMENT'),
		'string' => array('name' => 'varchar', 'length' => 255),
		'text' => array('name' => 'text'),
		'integer' => array('name' => 'int', 'length' => 11, 'formatter' => 'intval'),
		'float' => array('name' => 'float', 'formatter' => 'floatval'),
		'datetime' => array('name' => 'datetime', 'format' => 'Y-m-d H:i:s', 'formatter' => 'date'),
		'timestamp' => array(
			'name' => 'timestamp', 'format' => 'Y-m-d H:i:s', 'formatter' => 'date'
		),
		'time' => array('name' => 'time', 'format' => 'H:i:s', 'formatter' => 'date'),
		'date' => array('name' => 'date', 'format' => 'Y-m-d', 'formatter' => 'date'),
		'binary' => array('name' => 'blob'),
		'boolean' => array('name' => 'tinyint', 'length' => 1)
	);

	public $connection = null;

	public $sql = null;

	public $logs = array();

	public $log = false;

	public $return = array();

	protected $_quotes = array('{', '}');

	public function __construct(array $config = array()) {
		parent::__construct($config);
		$this->connection = $this;
	}

	public function quote($value) {
		return "'{$value}'";
	}

	public function connect() {
		return true;
	}

	public function disconnect() {
		return true;
	}

	public function sources($class = null) {}

	public function describe($entity, $schema = array(), array $meta = array()) {
		return $this->_instance('schema', array('fields' => $schema));
	}

	public function encoding($encoding = null) {}

	public function result($type, $resource, $context) {}

	public function error() {}

	public function value($value, array $schema = array()) {
		if (($result = parent::value($value, $schema)) !== null) {
			return $result;
		}
		return "'{$value}'";
	}

	public function cast($entity, array $data, array $options = array()) {
		$defaults = array('first' => false);
		$options += $defaults;
		return $options['first'] ? reset($data) : $data;
	}

	public function testConfig() {
		return $this->_config;
	}

	protected function _execute($sql) {
		$this->sql = $sql;
		if ($this->log) {
			$this->logs[] = $sql;
		}
		if (isset($this->return['_execute'])) {
			return $this->return['_execute'];
		}
		return new MockResult();
	}

	public function schema($query, $resource = null, $context = null) {
		if (isset($this->return['schema'])) {
			return $this->return['schema'];
		}
		return parent::schema($query, $resource = null, $context = null);
	}

	protected function _insertId($query) {
		$query = $query->export($this);
		ksort($query);
		return sha1(serialize($query));
	}
}

?>