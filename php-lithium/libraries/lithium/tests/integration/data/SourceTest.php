<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\integration\data;

use lithium\data\Connections;
use lithium\tests\mocks\data\Companies;
use lithium\tests\mocks\data\Employees;

class SourceTest extends \lithium\test\Integration {

	protected $_database = null;

	protected $_connection = null;

	protected $_classes = array(
		'employees' => 'lithium\tests\mocks\data\Employees',
		'companies' => 'lithium\tests\mocks\data\Companies'
	);

	public $companiesData = array(
		array('name' => 'StuffMart', 'active' => true),
		array('name' => 'Ma \'n Pa\'s Data Warehousing & Bait Shop', 'active' => false)
	);

	/**
	 * Creating the test database
	 */
	public function setUp() {
		$this->_connection->connection->put($this->_database);
	}

	/**
	 * Dropping the test database
	 */
	public function tearDown() {
		$this->_connection->connection->delete($this->_database);
	}

	/**
	 * Skip the test if no test database connection available.
	 */
	public function skip() {
		$connection = 'lithium_couch_test';
		$config = Connections::get($connection, array('config' => true));
		$isConnected = $config && Connections::get($connection)->isConnected(array(
			'autoConnect' => true
		));
		$isAvailable = $config && $isConnected;
		$this->skipIf(!$isAvailable, "No {$connection} connection available.");

		$this->_key = Companies::key();
		$this->_database = $config['database'];
		$this->_connection = Connections::get($connection);
	}

	/**
	 * Tests that a single record with a manually specified primary key can be created, persisted
	 * to an arbitrary data store, re-read and updated.
	 */
	public function testSingleReadWriteWithKey() {
		$key = Companies::meta('key');
		$new = Companies::create(array($key => 12345, 'name' => 'Acme, Inc.'));

		$result = $new->data();
		$expected = array($key => 12345, 'name' => 'Acme, Inc.');
		$this->assertEqual($expected[$key], $result[$key]);
		$this->assertEqual($expected['name'], $result['name']);

		$this->assertFalse($new->exists());
		$this->assertTrue($new->save());
		$this->assertTrue($new->exists());

		$existing = Companies::find(12345);
		$result = $existing->data();
		$this->assertEqual($expected[$key], $result[$key]);
		$this->assertEqual($expected['name'], $result['name']);
		$this->assertTrue($existing->exists());

		$existing->name = 'Big Brother and the Holding Companies';
		$result = $existing->save();
		$this->assertTrue($result);

		$existing = Companies::find(12345);
		$result = $existing->data();
		$expected['name'] = 'Big Brother and the Holding Companies';
		$this->assertEqual($expected[$key], $result[$key]);
		$this->assertEqual($expected['name'], $result['name']);

		$this->assertTrue($existing->delete());
	}

	public function testRewind() {
		$key = Companies::meta('key');
		$new = Companies::create(array($key => 12345, 'name' => 'Acme, Inc.'));

		$result = $new->data();
		$this->assertTrue($result !== null);
		$this->assertTrue($new->save());
		$this->assertTrue($new->exists());

		$result = Companies::all(12345);
		$this->assertTrue($result !== null);

		$result = $result->rewind();
		$this->assertTrue($result !== null);
		$this->assertTrue(!is_string($result));
	}

	public function testFindFirstWithFieldsOption() {
		return;
		$key = Companies::meta('key');
		$new = Companies::create(array($key => 1111, 'name' => 'Test find first with fields.'));
		$result = $new->data();

		$expected = array($key => 1111, 'name' => 'Test find first with fields.');
		$this->assertEqual($expected['name'], $result['name']);
		$this->assertEqual($expected[$key], $result[$key]);
		$this->assertFalse($new->exists());
		$this->assertTrue($new->save());
		$this->assertTrue($new->exists());

		$result = Companies::find('first', array('fields' => array('name')));
		$this->assertFalse(is_null($result));

		$this->skipIf(is_null($result), 'No result returned to test');
		$result = $result->data();
		$this->assertEqual($expected['name'], $result['name']);

		$this->assertTrue($new->delete());
	}

	public function testReadWriteMultiple() {
		$companies = array();
		$key = Companies::meta('key');

		foreach ($this->companiesData as $data) {
			$companies[] = Companies::create($data);
			$this->assertTrue(end($companies)->save());
			$this->assertTrue(end($companies)->$key);
		}

		$this->assertIdentical(2, Companies::count());
		$this->assertIdentical(1, Companies::count(array('active' => true)));
		$this->assertIdentical(1, Companies::count(array('active' => false)));
		$this->assertIdentical(0, Companies::count(array('active' => null)));
		$all = Companies::all();
		$this->assertIdentical(2, Companies::count());

		$expected = count($this->companiesData);
		$this->assertEqual($expected, $all->count());
		$this->assertEqual($expected, count($all));

		$id = (string) $all->first()->{$key};
		$this->assertTrue(strlen($id) > 0);
		$this->assertTrue($all->data());

		foreach ($companies as $companies) {
			$this->assertTrue($companies->delete());
		}
		$this->assertIdentical(0, Companies::count());
	}

	public function testEntityFields() {
		foreach ($this->companiesData as $data) {
			Companies::create($data)->save();
		}
		$all = Companies::all();

		$result = $all->first(function($doc) { return $doc->name === 'StuffMart'; });
		$this->assertEqual('StuffMart', $result->name);

		$result = $result->data();
		$this->assertEqual('StuffMart', $result['name']);

		$result = $all->next();
		$this->assertEqual('Ma \'n Pa\'s Data Warehousing & Bait Shop', $result->name);

		$result = $result->data();
		$this->assertEqual('Ma \'n Pa\'s Data Warehousing & Bait Shop', $result['name']);

		$this->assertNull($all->next());
	}

	/**
	 * Tests that a record can be created, saved, and subsequently re-read using a key
	 * auto-generated by the data source. Uses short-hand `find()` syntax which does not support
	 * compound keys.
	 *
	 * @return void
	 */
	public function testGetRecordByGeneratedId() {
		$key = Companies::meta('key');
		$companies = Companies::create(array('name' => 'Test Companies'));
		$this->assertTrue($companies->save());

		$id = (string) $companies->{$key};
		$companiesCopy = Companies::find($id)->data();
		$data = $companies->data();

		foreach ($data as $key => $value) {
			$this->assertTrue(isset($companiesCopy[$key]));
			$this->assertEqual($data[$key], $companiesCopy[$key]);
		}
	}

	/**
	 * Tests the default relationship information provided by the backend data source.
	 *
	 * @return void
	 */
	public function testDefaultRelationshipInfo() {
		$connection = $this->_connection;
		$message = "Relationships are not supported by this adapter.";
		$this->skipIf(!$connection::enabled('relationships'), $message);

		$this->assertEqual(array('Employeess'), array_keys(Companies::relations()));
		$this->assertEqual(array('Companies'), array_keys(Employees::relations()));

		$this->assertEqual(array('Employeess'), Companies::relations('hasMany'));
		$this->assertEqual(array('Companies'), Employees::relations('belongsTo'));

		$this->assertFalse(Companies::relations('belongsTo'));
		$this->assertFalse(Companies::relations('hasOne'));

		$this->assertFalse(Employees::relations('hasMany'));
		$this->assertFalse(Employees::relations('hasOne'));

		$result = Companies::relations('Employeess');

		$this->assertEqual('hasMany', $result->data('type'));
		$this->assertEqual($this->_classes['employees'], $result->data('to'));
	}

	public function testAbstractTypeHandling() {
		$key = Companies::meta('key');

		foreach ($this->companiesData as $data) {
			$companies[] = Companies::create($data);
			$this->assertTrue(end($companies)->save());
			$this->assertTrue(end($companies)->{$key});
		}

		foreach (Companies::all() as $companies) {
			$this->assertTrue($companies->delete());
		}
	}
}

?>