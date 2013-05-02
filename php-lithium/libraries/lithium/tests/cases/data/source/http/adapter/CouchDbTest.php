<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\data\source\http\adapter;

use lithium\data\model\Query;
use lithium\data\entity\Document;
use lithium\data\source\http\adapter\CouchDb;

class CouchDbTest extends \lithium\test\Unit {

	public $db;

	public $query;

	protected $_testConfig = array(
		'database' => 'lithium-test',
		'persistent' => false,
		'scheme' => 'tcp',
		'host' => 'localhost',
		'login' => 'root',
		'password' => '',
		'port' => 80,
		'timeout' => 2,
		'socket' => 'lithium\tests\mocks\data\source\http\adapter\MockSocket'
	);

	protected $_model = 'lithium\tests\mocks\data\source\http\adapter\MockCouchPost';

	public function setUp() {
		$this->db = new CouchDb(array('socket' => false));
		$model = $this->_model;
		$model::resetSchema();
		$model::$connection = $this->db;

		$entity = new Document(compact('model'));
		$type = 'create';
		$this->query = new Query(compact('model', 'entity', 'type'));
	}

	public function testAllMethodsNoConnection() {
		$this->assertTrue($this->db->connect());
		$this->assertTrue($this->db->disconnect());
		$this->assertFalse($this->db->get());
		$this->assertFalse($this->db->post());
		$this->assertFalse($this->db->put());
	}

	public function testConnect() {
		$this->db = new CouchDb($this->_testConfig);
		$result = $this->db->connect();
		$this->assertTrue($result);
	}

	public function testDisconnect() {
		$couchdb = new CouchDb($this->_testConfig);
		$couchdb->connect();

		$result = $couchdb->disconnect();
		$this->assertTrue($result);
	}

	public function testSources() {
		$couchdb = new CouchDb($this->_testConfig);
		$result = $couchdb->sources();
		$this->assertNull($result);
	}

	public function testDescribe() {
		$couchdb = new CouchDb($this->_testConfig);
		$this->assertTrue(is_object($couchdb->describe('companies')));
	}

	public function testItem() {
		$couchdb = new CouchDb($this->_testConfig);
		$data = array('_id' => 'a1', '_rev' => '1-2', 'author' => 'author 1', 'body' => 'body 1');
		$expected = array('id' => 'a1', 'rev' => '1-2', 'author' => 'author 1', 'body' => 'body 1');

		$item = $couchdb->item($this->query->model(), $data);
		$result = $item->data();
		$this->assertEqual($expected, $result);
	}

	public function testCreateNoId() {
		$couchdb = new CouchDb($this->_testConfig);
		$this->query->data(array('name' => 'Acme Inc.'));

		$result = $couchdb->create($this->query);
		$this->assertTrue($result);

		$expected = '/lithium-test';
		$result = $couchdb->last->request->path;
		$this->assertEqual($expected, $result);

		$expected = array();
		$result = $couchdb->last->request->query;
		$this->assertEqual($expected, $result);
	}

	public function testCreateWithId() {
		$couchdb = new CouchDb($this->_testConfig);
		$this->query->data(array('id' => 12345, 'name' => 'Acme Inc.'));

		$result = $couchdb->create($this->query);
		$this->assertTrue($result);

		$expected = '/lithium-test/12345';
		$result = $couchdb->last->request->path;
		$this->assertEqual($expected, $result);

		$expected = array();
		$result = $couchdb->last->request->query;
		$this->assertEqual($expected, $result);
	}

	public function testReadNoConditions() {
		$couchdb = new CouchDb($this->_testConfig);

		$result = $couchdb->read($this->query);
		$this->assertTrue($result);
		$this->assertEqual(array('total_rows' => 3, 'offset' => 0), $result->stats());

		$expected = '/lithium-test/_all_docs';
		$result = $couchdb->last->request->path;
		$this->assertEqual($expected, $result);

		$expected = '?include_docs=true';
		$result = $couchdb->last->request->queryString();
		$this->assertEqual($expected, $result);
	}

	public function testReadWithConditions() {
		$couchdb = new CouchDb($this->_testConfig);

		$this->query->conditions(array('id' => 12345));
		$result = $couchdb->read($this->query);
		$this->assertTrue($result);

		$expected = '/lithium-test/12345';
		$result = $couchdb->last->request->path;
		$this->assertEqual($expected, $result);

		$expected = '';
		$result = $couchdb->last->request->queryString();
		$this->assertEqual($expected, $result);

		$this->query->conditions(array('id' => 12345, 'path' => '/lithium-test/12345'));
		$result = $couchdb->read($this->query);
		$this->assertTrue($result);
	}

	public function testReadWithViewConditions() {
		$couchdb = new CouchDb($this->_testConfig);

		$this->query->conditions(array(
			'design' => 'latest', 'view' => 'all', 'limit' => 10, 'descending' => 'true'
		));
		$result = $couchdb->read($this->query);
		$this->assertEqual(array('total_rows' => 3, 'offset' => 0), $result->stats());

		$expected = array(
			'id' => 'a1', 'rev' => '1-1', 'author' => 'author 1', 'body' => 'body 1'
		);
		$result = $result->data();
		$this->assertEqual($expected, $result['a1']);

		$expected = '/lithium-test/_design/latest/_view/all/';
		$result = $couchdb->last->request->path;
		$this->assertEqual($expected, $result);

		$expected = '?limit=10&descending=true';
		$result = $couchdb->last->request->queryString();
		$this->assertEqual($expected, $result);
	}

	public function testUpdate() {
		$couchdb = new CouchDb($this->_testConfig);
		$this->query->data(array('id' => 12345, 'rev' => '1-1', 'title' => 'One'));

		$result = $couchdb->update($this->query);
		$this->assertTrue($result);

		$expected = '/lithium-test/12345';
		$result = $couchdb->last->request->path;
		$this->assertEqual($expected, $result);

		$expected = array();
		$result = $couchdb->last->request->query;
		$this->assertEqual($expected, $result);
	}

	public function testDelete() {
		$couchdb = new CouchDb($this->_testConfig);
		$this->query->data(array('id' => 12345, 'rev' => '1-1', 'name' => 'Acme Inc'));

		$result = $couchdb->delete($this->query);
		$this->assertTrue($result);

		$expected = '/lithium-test/12345';
		$result = $couchdb->last->request->path;
		$this->assertEqual($expected, $result);

		$expected = '?rev=1-1';
		$result = $couchdb->last->request->queryString();
		$this->assertEqual($expected, $result);
	}

	public function testEnabled() {
		$this->assertEqual(CouchDb::enabled(), true);

		$this->assertEqual(CouchDb::enabled('arrays'), true);
		$this->assertEqual(CouchDb::enabled('transactions'), false);
		$this->assertEqual(CouchDb::enabled('booleans'), true);
		$this->assertEqual(CouchDb::enabled('relationships'), false);
	}

	public function testRespondsTo() {
		$couchdb = new CouchDb($this->_testConfig);
		$this->assertTrue($couchdb->respondsTo('foobarbaz'));
		$this->assertFalse($couchdb->respondsTo(0));
	}

}

?>