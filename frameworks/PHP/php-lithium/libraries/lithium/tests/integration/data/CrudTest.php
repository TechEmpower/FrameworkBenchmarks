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

class CrudTest extends \lithium\test\Integration {

	protected $_connection = null;

	protected $_database = null;

	protected $_key = null;

	public $companyData = array(
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
	 *
	 * @return void
	 */
	public function testCreate() {
		$this->assertIdentical(0, Companies::count());

		$new = Companies::create(array('name' => 'Acme, Inc.', 'active' => true));
		$expected = array('name' => 'Acme, Inc.', 'active' => true);
		$result = $new->data();
		$this->assertEqual($expected, $result);

		$this->assertEqual(
			array(false, true, true),
			array($new->exists(), $new->save(), $new->exists())
		);
		$this->assertIdentical(1, Companies::count());
	}

	public function testRead() {
		static::_createCompany();
		$existing = Companies::first();

		foreach (Companies::key($existing) as $val) {
			$this->assertTrue($val);
		}
		$this->assertEqual('Acme, Inc.', $existing->name);
		$this->assertTrue($existing->active);
		$this->assertTrue($existing->exists());
	}

	public function testUpdate() {
		static::_createCompany();
		$existing = Companies::first();
		$this->assertEqual($existing->name, 'Acme, Inc.');
		$existing->name = 'Big Brother and the Holding Company';
		$result = $existing->save();
		$this->assertTrue($result);

		$existing = Companies::first();
		foreach (Companies::key($existing) as $val) {
			$this->assertTrue($val);
		}
		$this->assertTrue($existing->active);
		$this->assertEqual('Big Brother and the Holding Company', $existing->name);
	}

	public function testDelete() {
		static::_createCompany();
		$existing = Companies::first();
		$this->assertTrue($existing->exists());
		$this->assertTrue($existing->delete());
		$this->assertNull(Companies::first(array('conditions' => Companies::key($existing))));
		$this->assertIdentical(0, Companies::count());
	}

	public function testCrudMulti() {
		$large  = Companies::create(array('name' => 'BigBoxMart', 'active' => true));
		$medium = Companies::create(array('name' => 'Acme, Inc.', 'active' => true));
		$small  = Companies::create(array('name' => 'Ma & Pa\'s', 'active' => true));

		foreach (array('large', 'medium', 'small') as $key) {
			$this->assertFalse(${$key}->exists());
			$this->assertTrue(${$key}->save());
			$this->assertTrue(${$key}->exists());
		}
		$this->assertEqual(3, Companies::count());

		$all = Companies::all();
		$this->assertEqual(3, $all->count());

		$match = 'BigBoxMart';
		$filter = function($entity) use (&$match) { return $entity->name === $match; };

		foreach (array('BigBoxMart', 'Acme, Inc.', 'Ma & Pa\'s') as $match) {
			$this->assertTrue($all->first($filter)->exists());
		}
		$this->assertEqual(array(true, true, true), array_values($all->delete()));
		$this->assertEqual(0, Companies::count());
	}

	public function testUpdateWithNewProperties() {
		$new = Companies::create(array('name' => 'Acme, Inc.', 'active' => true));

		$expected = array('name' => 'Acme, Inc.', 'active' => true);
		$result = $new->data();
		$this->assertEqual($expected, $result);

		$new->foo = 'bar';
		$expected = array('name' => 'Acme, Inc.', 'active' => true, 'foo' => 'bar');
		$result = $new->data();
		$this->assertEqual($expected, $result);

		$this->assertTrue($new->save());

		$updated = Companies::find((string) $new->_id);
		$expected = 'bar';
		$result = $updated->foo;
		$this->assertEqual($expected, $result);
	}

	protected static function _createCompany() {
		Companies::create(array(
			'name' => 'Acme, Inc.',
			'active' => true
		))->save();
	}
}

?>