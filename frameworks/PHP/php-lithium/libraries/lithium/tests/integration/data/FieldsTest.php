<?php

namespace lithium\tests\integration\data;

use lithium\data\Connections;
use lithium\data\Entity;
use lithium\data\source\Database;
use lithium\tests\mocks\data\MockEmployees;
use lithium\tests\mocks\data\MockCompanies;

class FieldsTest extends \lithium\test\Integration {

	public $db = null;

	protected $_dbConfig = null;

	public function setUp() {
		$mockBase = LITHIUM_LIBRARY_PATH . '/lithium/tests/mocks/data/source/database/adapter/';
		$files = array('companies' => '_companies.sql', 'employees' => '_employees.sql');
		$files = array_diff_key($files, array_flip($this->db->sources()));

		foreach ($files as $file) {
			$sqlFile = $mockBase . strtolower($this->_dbConfig['adapter']) . $file;
			$this->skipIf(!file_exists($sqlFile), "SQL file $sqlFile does not exist.");
			$sql = file_get_contents($sqlFile);
			$this->db->read($sql, array('return' => 'resource'));
		}
	}

	public function tearDown() {
		$this->db->read('DROP TABLE IF EXISTS `employees`;');
		$this->db->read('DROP TABLE IF EXISTS `companies`;');
	}

	public function skip() {
		$connection = 'lithium_mysql_test';
		$this->_dbConfig = Connections::get($connection, array(
			'config' => true
		));
		$isConnected = $this->_dbConfig && Connections::get($connection)->isConnected(array(
			'autoConnect' => true
		));
		$isAvailable = $this->_dbConfig && $isConnected;
		$this->skipIf(!$isAvailable, "No {$connection} connection available.");

		$this->db = Connections::get($connection);
		$this->skipIf(
			!($this->db instanceof Database),
			"The {$connection} connection is not a relational database."
		);
	}

	public function testSingleField() {
		$new = MockCompanies::create(array('name' => 'Acme, Inc.'));
		$key = MockCompanies::meta('key');
		$new->save();
		$id = is_object($new->{$key}) ? (string) $new->{$key} : $new->{$key};

		$entity = MockCompanies::first($id);

		$this->assertTrue($entity instanceof Entity);
		$this->skipIf(!$entity instanceof Entity, 'Queried object is not an entity.');

		$expected = array(
			$key => $id, 'name' => 'Acme, Inc.', 'active' => null,
			'created' => null, 'modified' => null
		);
		$result = $entity->data();
		$this->assertEqual($expected, $result);

		$entity = MockCompanies::first(array(
			'conditions' => array($key => $id),
			'fields' => array($key)
		));

		$this->assertTrue($entity instanceof Entity);
		$this->skipIf(!$entity instanceof Entity, 'Queried object is not an entity.');

		$expected = array($key => $id);
		$result = $entity->data();
		$this->assertEqual($expected, $result);

		$entity = MockCompanies::find('first',array(
			'conditions' => array($key => $id),
			'fields' => array($key, 'name')
		));
		$this->assertTrue($entity instanceof Entity);
		$this->skipIf(!$entity instanceof Entity, 'Queried object is not an entity.');

		$entity->name = 'Acme, Incorporated';
		$result = $entity->save();
		$this->assertTrue($result);

		$entity = MockCompanies::find('first',array(
			'conditions' => array($key => $id),
			'fields' => array($key, 'name')
		));
		$this->assertEqual($entity->name, 'Acme, Incorporated');
		$new->delete();
	}

	public function testFieldsWithJoins() {
		$new = MockCompanies::create(array('name' => 'Acme, Inc.'));
		$cKey = MockCompanies::meta('key');
		$result = $new->save();
		$cId = (string) $new->{$cKey};

		$this->skipIf(!$result, 'Could not save MockCompanies');

		$new = MockEmployees::create(array(
			'company_id' => $cId,
			'name' => 'John Doe'
		));
		$eKey = MockEmployees::meta('key');
		$result = $new->save();
		$this->skipIf(!$result, 'Could not save MockEmployee');
		$eId = (string) $new->{$eKey};

		$entity = MockCompanies::first(array(
			'with' => 'Employees',
			'conditions' => array(
				'MockCompanies.id' => $cId
			),
			'fields' => array(
				'MockCompanies' => array('id', 'name'),
				'Employees' => array('id', 'name')
			)
		));
		$expected = array(
			'id' => $cId,
			'name' => 'Acme, Inc.',
			'employees' => array (
				$eId => array (
					'id' => $eId,
					'name' => 'John Doe'
				)
			)
		);
		$this->assertEqual($expected, $entity->data());
	}
}

?>