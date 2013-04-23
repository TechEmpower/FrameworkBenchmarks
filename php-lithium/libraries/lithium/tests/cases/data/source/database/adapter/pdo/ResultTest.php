<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\data\source\database\adapter\pdo;

use PDOStatement;
use lithium\data\Connections;
use lithium\data\source\database\adapter\MySql;
use lithium\data\source\database\adapter\PostgreSql;
use lithium\data\source\database\adapter\pdo\Result;

class ResultTest extends \lithium\test\Unit {

	public $db = null;
	public $mockPrefix = '';

	protected $_mockData = array(
		1 => array(1, 'Foo Company'),
		2 => array(2, 'Bar Company')
	);

	/**
	 * Skip the test if a MySQL or PostgreSQL adapter configuration is unavailable and
	 * preload test data.
	 */
	public function skip() {
		$enabled = (MySql::enabled() || PostgreSql::enabled());
		$this->skipIf(!$enabled, 'MySQL or PostgreSQL Extension is not loaded');

		$dbConfig = Connections::get('test', array('config' => true));
		$validAdapter = in_array($dbConfig['adapter'], array('MySql', 'PostgreSql'));
		$hasDb = (isset($dbConfig['adapter']) && $validAdapter);
		$message = 'Test database is either unavailable, or not using a MySQL/PostgreSQL adapter';
		$this->skipIf(!$hasDb, $message);

		switch ($dbConfig['adapter']) {
			case "MySql":
				$this->db = new MySql($dbConfig);
				$this->mockPrefix = 'mysql';
			break;
			case "PostgreSql":
				$this->db = new PostgreSql($dbConfig);
				$this->mockPrefix = 'postgresql';
			break;
		}
	}

	public function setUp() {
		$lithium = LITHIUM_LIBRARY_PATH . '/lithium';
		$prefix = $this->mockPrefix;
		$sqlFile = $lithium . "/tests/mocks/data/source/database/adapter/{$prefix}_companies.sql";
		$sql = file_get_contents($sqlFile);
		$this->db->read($sql, array('return' => 'resource'));

		foreach ($this->_mockData as $entry) {
			$sql = "INSERT INTO companies (name) VALUES ('" . $entry[1] . "')";
			$this->db->read($sql, array('return' => 'resource'));
		}
	}

	public function tearDown() {
		$this->db->read('DROP TABLE companies', array('return' => 'resource'));
	}

	public function testConstruct() {
		$result = new Result();
		$this->assertNull($result->resource());

		$resource = new PDOStatement;
		$result = new Result(compact('resource'));
		$this->assertTrue($result->resource() instanceof PDOStatement);
	}

	public function testNext() {
		$resource = $this->db->connection->query("SELECT id, name FROM companies;");
		$result = new Result(compact('resource'));

		$this->assertEqual($this->_mockData[1], $result->next());
		$this->assertEqual($this->_mockData[2], $result->next());
		$this->assertFalse($result->next());
	}

	public function testPrev() {
		$resource = $this->db->connection->query("SELECT id, name FROM companies;");
		$result = new Result(compact('resource'));

		$this->assertNull($result->prev());
		$this->assertEqual($this->_mockData[1], $result->next());
		$this->assertEqual($this->_mockData[2], $result->next());
		$this->assertEqual($this->_mockData[1], $result->prev());
		$this->assertEqual($this->_mockData[2], $result->next());
		$this->assertEqual($this->_mockData[1], $result->prev());
		$this->assertFalse($result->prev());
	}

	public function testValid() {
		$result = new Result();
		$this->assertFalse($result->valid());

		$resource = $this->db->connection->query("SELECT id, name FROM companies;");
		$result = new Result(compact('resource'));
		$this->assertTrue($result->valid());
	}

	public function testRewind() {
		$resource = $this->db->connection->query("SELECT id, name FROM companies;");
		$result = new Result(compact('resource'));

		$this->assertEqual($this->_mockData[1], $result->next());
		$this->assertEqual($this->_mockData[2], $result->next());
		$result->rewind();
		$this->assertEqual($this->_mockData[1], $result->current());
	}

	public function testCurrent() {
		$resource = $this->db->connection->query("SELECT id, name FROM companies;");
		$result = new Result(compact('resource'));

		$this->assertEqual($this->_mockData[1], $result->next());
		$this->assertEqual($this->_mockData[1], $result->current());
		$this->assertEqual($this->_mockData[2], $result->next());
		$this->assertEqual($this->_mockData[2], $result->current());
		$this->assertEqual($this->_mockData[1], $result->prev());
		$this->assertEqual($this->_mockData[1], $result->current());
	}

	public function testKey() {
		$resource = $this->db->connection->query("SELECT id, name FROM companies;");
		$result = new Result(compact('resource'));

		$this->assertIdentical(0, $result->key());
		$result->next();
		$this->assertIdentical(1, $result->key());
		$result->prev();
		$this->assertIdentical(0, $result->key());
		$result->next();
		$this->assertIdentical(1, $result->key());
		$result->next();
		$this->assertIdentical(null, $result->key());
		$result->rewind();
		$this->assertIdentical(0, $result->key());
	}

	/**
	 * Test that a Result object can be used in a foreach loop
	 */
	public function testResultForeach() {

		$result = $this->db->read('SELECT name, active FROM companies', array(
			'return' => 'resource'
		));

		$rows = array();
		foreach ($result as $row) {
			$rows[] = $row;
		}

		$expected = array(
			array('Foo Company', null),
			array('Bar Company', null)
		);

		$this->assertEqual($expected, $rows);
	}

	/**
	 * Test that an empty Result object can be used in a foreach loop
	 */
	public function testEmptyResultForeach() {

		$this->db->delete('DELETE FROM companies');

		$result = $this->db->read('SELECT name, active FROM companies', array(
			'return' => 'resource'
		));

		$rows = array();
		foreach ($result as $row) {
			$rows[] = $row;
		}

		$expected = array();

		$this->assertEqual($expected, $rows);
	}
}

?>