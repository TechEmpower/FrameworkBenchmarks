<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\data\source\database\adapter;

use lithium\data\Connections;
use lithium\data\model\Query;
use lithium\data\source\database\adapter\MySql;
use lithium\tests\mocks\data\source\database\adapter\MockMySql;

class MySqlTest extends \lithium\test\Unit {

	protected $_dbConfig = array();

	public $db = null;

	/**
	 * Skip the test if a MySQL adapter configuration is unavailable.
	 * @todo Tie into the Environment class to ensure that the test database is being used.
	 */
	public function skip() {
		$this->skipIf(!MySql::enabled(), 'MySQL Extension is not loaded');

		$this->_dbConfig = Connections::get('test', array('config' => true));
		$hasDb = (isset($this->_dbConfig['adapter']) && $this->_dbConfig['adapter'] === 'MySql');
		$message = 'Test database is either unavailable, or not using a MySQL adapter';
		$this->skipIf(!$hasDb, $message);

		$this->db = new MySql($this->_dbConfig);

		$lithium = LITHIUM_LIBRARY_PATH . '/lithium';
		$sqlFile = $lithium . '/tests/mocks/data/source/database/adapter/mysql_companies.sql';
		$sql = file_get_contents($sqlFile);
		$this->db->read($sql, array('return' => 'resource'));
	}

	/**
	 * Tests that the object is initialized with the correct default values.
	 */
	public function testConstructorDefaults() {
		$db = new MockMySql(array('autoConnect' => false));
		$result = $db->get('_config');
		$expected = array(
			'autoConnect' => false, 'encoding' => null,'persistent' => true,
			'host' => 'localhost:3306', 'login' => 'root', 'password' => '',
			'database' => null, 'dsn' => null, 'options' => array(), 'init' => true
		);
		$this->assertEqual($expected, $result);
	}

	/**
	 * Tests that this adapter can connect to the database, and that the status is properly
	 * persisted.
	 */
	public function testDatabaseConnection() {
		$db = new MySql(array('autoConnect' => false) + $this->_dbConfig);

		$this->assertTrue($db->connect());
		$this->assertTrue($db->isConnected());

		$this->assertTrue($db->disconnect());
		$this->assertFalse($db->isConnected());

		$db = new MySQL(array(
			'autoConnect' => false, 'encoding' => null,'persistent' => false,
			'host' => 'localhost:3306', 'login' => 'garbage', 'password' => '',
			'database' => 'garbage', 'init' => true
		) + $this->_dbConfig);

		$this->expectException();
		$this->assertFalse($db->connect());
		$this->assertFalse($db->isConnected());

		$this->assertTrue($db->disconnect());
		$this->assertFalse($db->isConnected());
	}

	public function testDatabaseEncoding() {
		$this->assertTrue($this->db->isConnected());
		$this->assertTrue($this->db->encoding('utf8'));
		$this->assertEqual('UTF-8', $this->db->encoding());

		$this->assertTrue($this->db->encoding('UTF-8'));
		$this->assertEqual('UTF-8', $this->db->encoding());
	}

	public function testValueByIntrospect() {
		$expected = "'string'";
		$result = $this->db->value("string");
		$this->assertTrue(is_string($result));
		$this->assertEqual($expected, $result);

		$expected = "'\'this string is escaped\''";
		$result = $this->db->value("'this string is escaped'");
		$this->assertTrue(is_string($result));
		$this->assertEqual($expected, $result);

		$this->assertIdentical(1, $this->db->value(true));
		$this->assertIdentical(1, $this->db->value('1'));
		$this->assertIdentical(1.1, $this->db->value('1.1'));
	}

	public function testColumnAbstraction() {
		$result = $this->db->invokeMethod('_column', array('varchar'));
		$this->assertIdentical(array('type' => 'string'), $result);

		$result = $this->db->invokeMethod('_column', array('tinyint(1)'));
		$this->assertIdentical(array('type' => 'boolean'), $result);

		$result = $this->db->invokeMethod('_column', array('varchar(255)'));
		$this->assertIdentical(array('type' => 'string', 'length' => 255), $result);

		$result = $this->db->invokeMethod('_column', array('text'));
		$this->assertIdentical(array('type' => 'text'), $result);

		$result = $this->db->invokeMethod('_column', array('text'));
		$this->assertIdentical(array('type' => 'text'), $result);

		$result = $this->db->invokeMethod('_column', array('decimal(12,2)'));
		$this->assertIdentical(array('type' => 'float', 'length' => 12, 'precision' => 2), $result);

		$result = $this->db->invokeMethod('_column', array('int(11)'));
		$this->assertIdentical(array('type' => 'integer', 'length' => 11), $result);
	}

	public function testRawSqlQuerying() {
		$this->assertTrue($this->db->create(
			'INSERT INTO companies (name, active) VALUES (?, ?)',
			array('Test', 1)
		));

		$result = $this->db->read('SELECT * From companies AS Company WHERE name = {:name}', array(
			'name' => 'Test',
			'return' => 'array'
		));
		$this->assertEqual(1, count($result));
		$expected = array('id', 'name', 'active', 'created', 'modified');
		$this->assertEqual($expected, array_keys($result[0]));

		$this->assertTrue(is_numeric($result[0]['id']));
		unset($result[0]['id']);

		$expected = array('name' => 'Test', 'active' => '1', 'created' => null, 'modified' => null);
		$this->assertIdentical($expected, $result[0]);

		$this->assertTrue($this->db->delete('DELETE From companies WHERE name = {:name}', array(
			'name' => 'Test'
		)));

		$result = $this->db->read('SELECT * From companies AS Company WHERE name = {:name}', array(
			'name' => 'Test',
			'return' => 'array'
		));
		$this->assertFalse($result);
	}

	public function testAbstractColumnResolution() {
	}

	public function testExecuteException() {
		$this->expectException();
		$this->db->read('SELECT deliberate syntax error');
	}

	public function testEnabledFeatures() {
		$this->assertTrue(MySql::enabled());
		$this->assertTrue(MySql::enabled('relationships'));
		$this->assertFalse(MySql::enabled('arrays'));
	}

	public function testEntityQuerying() {
		$sources = $this->db->sources();
		$this->assertTrue(is_array($sources));
		$this->assertFalse(empty($sources));
	}

	public function testQueryOrdering() {
		$insert = new Query(array(
			'type' => 'create',
			'source' => 'companies',
			'data' => array(
				'name' => 'Foo',
				'active' => true,
				'created' => date('Y-m-d H:i:s')
			)
		));
		$this->assertIdentical(true, $this->db->create($insert));

		$insert->data(array(
			'name' => 'Bar',
			'created' => date('Y-m-d H:i:s', strtotime('-5 minutes'))
		));
		$this->assertIdentical(true, $this->db->create($insert));

		$insert->data(array(
			'name' => 'Baz',
			'created' => date('Y-m-d H:i:s', strtotime('-10 minutes'))
		));
		$this->assertIdentical(true, $this->db->create($insert));

		$read = new Query(array(
			'type' => 'read',
			'source' => 'companies',
			'fields' => array('name'),
			'order' => array('created' => 'asc')
		));
		$result = $this->db->read($read, array('return' => 'array'));
		$expected = array(
			array('name' => 'Baz'),
			array('name' => 'Bar'),
			array('name' => 'Foo')
		);
		$this->assertEqual($expected, $result);

		$read->order(array('created' => 'desc'));
		$result = $this->db->read($read, array('return' => 'array'));
		$expected = array(
			array('name' => 'Foo'),
			array('name' => 'Bar'),
			array('name' => 'Baz')
		);
		$this->assertEqual($expected, $result);

		$delete = new Query(array('type' => 'delete', 'source' => 'companies'));
		$this->assertTrue($this->db->delete($delete));
	}

	/**
	 * Ensures that DELETE queries are not generated with table aliases, as MySQL does not support
	 * this.
	 */
	public function testDeletesWithoutAliases() {
		$delete = new Query(array('type' => 'delete', 'source' => 'companies'));
		$this->assertTrue($this->db->delete($delete));
	}

	/**
	 * Tests that describing a table's schema returns the correct column meta-information.
	 */
	public function testDescribe() {
		$result = $this->db->describe('companies')->fields();
		$expected = array(
			'id' => array('type' => 'integer', 'length' => 11, 'null' => false, 'default' => null),
			'name' => array('type' => 'string', 'length' => 255, 'null' => true, 'default' => null),
			'active' => array('type' => 'boolean', 'null' => true, 'default' => null),
			'created' => array('type' => 'datetime', 'null' => true, 'default' => null),
			'modified' => array('type' => 'datetime', 'null' => true, 'default' => null)
		);
		$this->assertEqual($expected, $result);

		unset($expected['name']);
		unset($expected['modified']);
		$result = $this->db->describe('companies', $expected)->fields();
		$this->assertEqual($expected, $result);
	}
}

?>