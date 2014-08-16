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
use lithium\data\source\database\adapter\PostgreSql;
use lithium\tests\mocks\data\source\database\adapter\MockPostgreSql;

class PostgreSqlTest extends \lithium\test\Unit {

	protected $_dbConfig = array();

	protected $_model = 'lithium\tests\mocks\data\model\MockDatabasePost';

	public $db = null;

	/**
	 * Skip the test if a PostgreSQL adapter configuration is unavailable.
	 * @todo Tie into the Environment class to ensure that the test database is being used.
	 */
	public function skip() {
		$this->skipIf(!PostgreSql::enabled(), 'PostgreSQL Extension is not loaded');

		$adapter = 'PostgreSql';
		$this->_dbConfig = Connections::get('test', array('config' => true));
		$hasDb = (isset($this->_dbConfig['adapter']) && $this->_dbConfig['adapter'] === $adapter);
		$message = 'Test database is either unavailable, or not using a PostgreSQL adapter';
		$this->skipIf(!$hasDb, $message);

		$this->db = new PostgreSql($this->_dbConfig);

		$lithium = LITHIUM_LIBRARY_PATH . '/lithium';
		$sqlFile = $lithium . '/tests/mocks/data/source/database/adapter/postgresql_companies.sql';
		$sql = file_get_contents($sqlFile);
		$this->db->read($sql, array('return' => 'resource'));
	}

	/**
	 * Tests that the object is initialized with the correct default values.
	 */
	public function testConstructorDefaults() {
		$db = new MockPostgreSql(array('autoConnect' => false));
		$result = $db->get('_config');
		$expected = array(
			'autoConnect' => false, 'encoding' => null,'persistent' => true,
			'host' => 'localhost:5432', 'login' => 'root', 'password' => '',
			'database' => null, 'dsn' => null, 'options' => array(),
			'init' => true, 'schema' => 'public', 'timezone' => null
		);
		$this->assertEqual($expected, $result);
	}

	/**
	 * Tests that this adapter can connect to the database, and that the status is properly
	 * persisted.
	 */
	public function testDatabaseConnection() {
		$db = new PostgreSql(array('autoConnect' => false) + $this->_dbConfig);

		$this->assertTrue($db->connect());
		$this->assertTrue($db->isConnected());

		$this->assertTrue($db->disconnect());
		$this->assertFalse($db->isConnected());

		$db = new PostgreSql(array(
			'autoConnect' => false, 'encoding' => null,'persistent' => false,
			'host' => 'localhost:5432', 'login' => 'garbage', 'password' => '',
			'database' => 'garbage', 'init' => true, 'schema' => 'garbage'
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

	public function testDatabaseTimezone() {
		$this->assertTrue($this->db->isConnected());
		$this->assertTrue($this->db->timezone('UTC'));
		$this->assertEqual('UTC', $this->db->timezone());

		$this->assertTrue($this->db->timezone('US/Eastern'));
		$this->assertEqual('US/Eastern', $this->db->timezone());
	}

	public function testValueByIntrospect() {
		$expected = "'string'";
		$result = $this->db->value("string");
		$this->assertTrue(is_string($result));
		$this->assertEqual($expected, $result);

		$expected = "'''this string is escaped'''";
		$result = $this->db->value("'this string is escaped'");
		$this->assertTrue(is_string($result));
		$this->assertEqual($expected, $result);

		$this->assertIdentical("'t'", $this->db->value(true));
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
			array('Test', "t")
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

		$expected = array(
			'name' => 'Test',
			'active' => true,
			'created' => null,
			'modified' => null
		);
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
		$this->assertTrue(PostgreSql::enabled());
		$this->assertTrue(PostgreSql::enabled('relationships'));
		$this->assertFalse(PostgreSql::enabled('arrays'));
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
	 * Ensures that DELETE queries are not generated with table aliases, as PostgreSQL does not
	 * support this.
	 */
	public function testDeletesWithoutAliases() {
		$delete = new Query(array('type' => 'delete', 'source' => 'companies'));
		$this->assertTrue($this->db->delete($delete));
	}

	/**
	 * Tests that describing a table's schema returns the correct column meta-information.
	 */
	public function testDescribe() {
		$result = $this->db->describe('companies');
		$expected = array(
			'id' => array(
				'type' => 'integer', 'null' => false,
				'default' => 'nextval(\'companies_id_seq\'::regclass)'
			),
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