<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\data\source;

use lithium\data\source\mongo_db\Schema;
use lithium\data\source\MongoDb;
use MongoId;
use MongoCode;
use MongoDate;
use MongoRegex;
use lithium\data\model\Query;
use lithium\data\entity\Document;
use lithium\tests\mocks\data\MockPost;
use lithium\tests\mocks\data\MockComment;
use lithium\data\collection\DocumentSet;
use lithium\tests\mocks\core\MockCallable;
use lithium\tests\mocks\data\source\MockMongoSource;
use lithium\tests\mocks\data\source\MockMongoConnection;
use lithium\tests\mocks\data\source\mongo_db\MockResult;

class MongoDbTest extends \lithium\test\Unit {

	protected $_model = 'lithium\tests\mocks\data\source\MockMongoPost';

	protected $_testConfig = array(
		'adapter' => false,
		'database' => 'lithium_test',
		'host' => 'localhost',
		'port' => '27017',
		'persistent' => null,
		'autoConnect' => false
	);

	protected $_schema = array(
		'_id'               => 'id',
		'guid'              => 'id',
		'title'             => 'string',
		'tags'              => array('type' => 'string', 'array' => true),
		'comments'          => 'MongoId',
		'authors'           => array('type' => 'MongoId', 'array' => true),
		'created'           => 'MongoDate',
		'modified'          => 'datetime',
		'voters'            => array('type' => 'id', 'array' => true),
		'rank_count'        => array('type' => 'integer', 'default' => 0),
		'rank'              => array('type' => 'float', 'default' => 0.0),
		'notifications.foo' => 'boolean',
		'notifications.bar' => 'boolean',
		'notifications.baz' => 'boolean'
	);

	protected $_configs = array();

	public function skip() {
		$this->skipIf(!MongoDb::enabled(), 'The `MongoDb` class is not enabled.');
	}

	public function setUp() {
		$model = $this->_model;
		$this->db = new MongoDb($this->_testConfig);
		$this->db->server = new MockMongoConnection();
		$this->db->connection = new MockMongoConnection();
		$this->db->server->connected = true;

		$model::config(array('meta' => array('key' => '_id')));
		$model::$connection = $this->db;
		$type = 'create';

		$this->query = new Query(compact('model', 'type') + array(
			'entity' => new Document(compact('model'))
		));
	}

	public function tearDown() {
		unset($this->query);
		MockPost::reset();
		MockComment::reset();
	}

	public function testBadConnection() {
		$db = new MongoDb(array('host' => null, 'autoConnect' => false));
		$this->expectException('Could not connect to the database.');
		$this->assertFalse($db->connect());
		$this->assertTrue($db->disconnect());
	}

	public function testGoodConnectionBadDatabase() {
		$this->expectException('Could not connect to the database.');
		$db = new MongoDb(array('database' => null, 'autoConnnect' => false));
	}

	public function testSources() {
		$this->db->connection->results = array(array());
		$this->assertEqual(array(), $this->db->sources());
	}

	public function testDescribe() {
		$result = $this->db->describe('test')->fields();
		$expected = array('_id' => array('type' => 'id'));
		$this->assertEqual($expected, $result);
	}

	public function testName() {
		$result = $this->db->name('{(\'Li\':"∆")}');
		$expected = '{(\'Li\':"∆")}';
		$this->assertEqual($expected, $result);
	}

	public function testSchema() {
		$result = $this->db->schema($this->query);
		$expected = array();
		$this->assertEqual($expected, $result);
	}

	public function testCreateSuccess() {
		array_push($this->db->connection->results, true);
		$this->query->data(array('title' => 'Test Post'));
		$this->assertTrue($this->db->create($this->query));

		$query = array_pop($this->db->connection->queries);

		$this->assertFalse($this->db->connection->queries);
		$this->assertEqual('insert', $query['type']);
		$this->assertEqual('posts', $query['collection']);
		$this->assertEqual(array('title', '_id'), array_keys($query['data']));
		$this->assertTrue($query['data']['_id'] instanceof MongoId);
	}

	public function testConditions() {
		$result = $this->db->conditions(null, null);
		$this->assertEqual(array(), $result);

		$function = 'function() { return this.x < y;}';
		$conditions = new MongoCode($function);
		$result = $this->db->conditions($conditions, null);

		$this->assertTrue(is_array($result));
		$this->assertTrue(isset($result['$where']));
		$this->assertEqual($conditions, $result['$where']);

		$conditions = $function;
		$result = $this->db->conditions($conditions, null);
		$this->assertTrue(is_array($result));
		$this->assertTrue(isset($result['$where']));
		$this->assertEqual($conditions, $result['$where']);

		$conditions = array('key' => 'value', 'anotherkey' => 'some other value');
		$result = $this->db->conditions($conditions, null);
		$this->assertTrue(is_array($result));
		$this->assertEqual($conditions, $result);

		$conditions = array('key' => array('one', 'two', 'three'));
		$result = $this->db->conditions($conditions, null);
		$this->assertTrue(is_array($result));
		$this->assertTrue(isset($result['key']));
		$this->assertTrue(isset($result['key']['$in']));
		$this->assertEqual($conditions['key'], $result['key']['$in']);

		$conditions = array('$or' => array(
			array('key' => 'value'),
			array('other key' => 'another value')
		));
		$result = $this->db->conditions($conditions, null);
		$this->assertTrue(isset($result['$or']));
		$this->assertEqual($conditions['$or'][0]['key'], $result['$or'][0]['key']);

		$conditions = array('$and' => array(
			array('key' => 'value'),
			array('other key' => 'another value')
		));
		$result = $this->db->conditions($conditions, null);
		$this->assertTrue(isset($result['$and']));
		$this->assertEqual($conditions['$and'][0]['key'], $result['$and'][0]['key']);

		$conditions = array('$nor' => array(
			array('key' => 'value'),
			array('other key' => 'another value')
		));
		$result = $this->db->conditions($conditions, null);
		$this->assertTrue(isset($result['$nor']));
		$this->assertEqual($conditions['$nor'][0]['key'], $result['$nor'][0]['key']);

		$conditions = array('key' => array('or' => array(1, 2)));
		$result = $this->db->conditions($conditions, null);
		$this->assertEqual(array('key' => array('$or' => array(1, 2))), $result);
	}

	public function testMongoConditionalOperators() {
		$conditions = array('key' => array('<' => 10));
		$expected = array('key' => array('$lt' => 10));
		$result = $this->db->conditions($conditions, null);
		$this->assertEqual($expected, $result);

		$conditions = array('key' => array('<=' => 10));
		$expected = array('key' => array('$lte' => 10));
		$result = $this->db->conditions($conditions, null);
		$this->assertEqual($expected, $result);

		$conditions = array('key' => array('>' => 10));
		$expected = array('key' => array('$gt' => 10));
		$result = $this->db->conditions($conditions, null);
		$this->assertEqual($expected, $result);

		$conditions = array('key' => array('>=' => 10));
		$expected = array('key' => array('$gte' => 10));
		$result = $this->db->conditions($conditions, null);
		$this->assertEqual($expected, $result);

		$conditions = array('key' => array('!=' => 10));
		$expected = array('key' => array('$ne' => 10));
		$result = $this->db->conditions($conditions, null);
		$this->assertEqual($expected, $result);

		$conditions = array('key' => array('<>' => 10));
		$expected = array('key' => array('$ne' => 10));
		$result = $this->db->conditions($conditions, null);
		$this->assertEqual($expected, $result);

		$conditions = array('key' => array('!=' => array(10, 20, 30)));
		$expected = array('key' => array('$nin' => array(10, 20, 30)));
		$result = $this->db->conditions($conditions, null);
		$this->assertEqual($expected, $result);

		$conditions = array('key' => array('<>' => array(10, 20, 30)));
		$expected = array('key' => array('$nin' => array(10, 20, 30)));
		$result = $this->db->conditions($conditions, null);
		$this->assertEqual($expected, $result);

		$conditions = array('key' => array('like' => '/regex/i'));
		$result = $this->db->conditions($conditions, null);
		$expected = array('key' => new MongoRegex('/regex/i'));
		$this->assertEqual($expected, $result);
	}

	public function testReadNoConditions() {
		$this->db->connect();
		$connection = $this->db->connection;
		$this->db->connection = new MockMongoSource();
		$this->db->connection->resultSets = array(array('ok' => true));

		$data = array('title' => 'Test Post');
		$options = array('safe' => false, 'fsync' => false);
		$this->query->data($data);
		$this->assertIdentical(true, $this->db->create($this->query));
		$this->assertEqual(compact('data', 'options'), end($this->db->connection->queries));

		$this->db->connection->resultSets = array(array(array('_id' => new MongoId()) + $data));
		$result = $this->db->read($this->query);

		$this->assertTrue($result instanceof DocumentSet);
		$this->assertEqual(1, $result->count());
		$this->assertEqual('Test Post', $result->first()->title);
		$this->db->connection = $connection;
	}

	public function testReadWithConditions() {
		$this->db->connect();
		$connection = $this->db->connection;
		$this->db->connection = new MockMongoSource();
		$this->db->connection->resultSets = array(array('ok' => true));

		$data = array('title' => 'Test Post');
		$options = array('safe' => false, 'fsync' => false);
		$this->query->data($data);
		$this->assertTrue($this->db->create($this->query));
		$this->query->data(null);

		$this->db->connection->resultSets = array(array());
		$this->query->conditions(array('title' => 'Nonexistent Post'));
		$result = $this->db->read($this->query);
		$this->assertTrue($result);
		$this->assertEqual(0, $result->count());

		$this->db->connection->resultSets = array(array($data));
		$this->query->conditions($data);
		$result = $this->db->read($this->query);
		$this->assertTrue($result);
		$this->assertEqual(1, $result->count());
		$this->db->connection = $connection;
	}

	public function testUpdate() {
		$model = $this->_model;
		$data = array('title' => 'Test Post');

		$this->query->model($model);
		$this->query->data($data);
		$this->db->connection->results = array(true);
		$this->db->create($this->query);

		$result = array_pop($this->db->connection->queries);
		$data['_id'] = $result['data']['_id'];

		$expected = compact('data') + array(
			'collection' => 'posts',
			'type' => 'insert',
			'options' => array('safe' => false, 'fsync' => false)
		);
		$this->assertEqual($expected, $result);

		$this->db->connection->results = array(
			new MockResult(array('data' => array($data))),
			new MockResult(array('data' => array($data)))
		);
		$this->db->connection->queries = array();

		$result = $this->db->read(new Query(compact('model')));
		$original = $result->first()->to('array');

		$this->assertEqual(array('title', '_id'), array_keys($original));
		$this->assertEqual('Test Post', $original['title']);
		$this->assertPattern('/^[0-9a-f]{24}$/', $original['_id']);

		$this->db->connection->results = array(true);
		$this->db->connection->queries = array();
		$update = array('title' => 'New Post Title');

		$this->query = new Query(compact('model') + array(
			'data' => $update,
			'conditions' => array('_id' => $original['_id'])
		));
		$this->assertTrue($this->db->update($this->query));

		$result = array_pop($this->db->connection->queries);
		$expected = array(
			'type' => 'update',
			'collection' => 'posts',
			'conditions' => array('_id' => '4f188fb17675ab167900010e'),
			'update' => array('$set' => array('title' => 'New Post Title')),
			'options' => array(
				'upsert' => false, 'multiple' => true, 'safe' => false, 'fsync' => false
			)
		);

		array_push($this->db->connection->results, new MockResult(array(
			'data' => array($update + $original)
		)));
		$this->db->connection->queries = array();

		$result = $this->db->read(new Query(compact('model') + array(
			'conditions' => array('_id' => $original['_id'])
		)));
		$this->assertEqual(1, $result->count());

		$updated = $result->first();
		$updated = $updated ? $updated->to('array') : array();
		$this->assertEqual($original['_id'], $updated['_id']);
		$this->assertEqual('New Post Title', $updated['title']);

		$expected = array(
			'type' => 'find',
			'collection' => 'posts',
			'fields' => array(),
			'conditions' => array('_id' => $original['_id'])
		);
		$this->assertEqual($expected, array_pop($this->db->connection->queries));
	}

	public function testDelete() {
		$data = array('title' => 'Delete Me');

		array_push($this->db->connection->results, true);
		$this->query->data($data);
		$this->db->create($this->query);

		array_push($this->db->connection->results, new MockResult(array(
			'data' => array()
		)));
		$this->assertFalse($this->db->read($this->query)->first());

		$result = array_pop($this->db->connection->queries);
		$conditions = array('_id' => $this->query->entity()->_id);
		$this->assertEqual($conditions, $result['conditions']);
		$this->assertTrue($this->query->entity()->exists());

		$model = $this->_model;
		$id = new MongoId();
		$this->query = new Query(compact('model') + array(
			'entity' => new Document(compact('model') + array('data' => array('_id' => $id)))
		));

		array_push($this->db->connection->results, true);
		$this->query->conditions($conditions);
		$this->assertTrue($this->db->delete($this->query));
		$this->assertFalse($this->query->entity()->exists());

		$expected = compact('conditions') + array(
			'type' => 'remove',
			'collection' => 'posts',
			'options' => array('justOne' => false, 'safe' => false, 'fsync' => false)
		);
		$this->assertEqual($expected, array_pop($this->db->connection->queries));
	}

	public function testItem() {
		$model = $this->_model;
		$data = array('title' => 'New Item');
		$result = $this->db->item($model, $data);
		$this->assertTrue($result instanceof Document);

		$expected = $data;
		$result = $result->to('array');
		$this->assertEqual($expected, $result);
	}

	public function testCalculation() {
		$this->db->connection->results = array(new MockResult(array('data' => array(5))));
		$this->assertIdentical(5, $this->db->calculation('count', $this->query));
	}

	public function testEnabled() {
		$this->assertTrue(MongoDb::enabled());
		$this->assertTrue(MongoDb::enabled('arrays'));
		$this->assertTrue(MongoDb::enabled('booleans'));
		$this->assertTrue(MongoDb::enabled('relationships'));
	}

	public function testArbitraryMethodCalls() {
		$this->assertTrue(is_array($this->db->listDBs()));
	}

	public function testDocumentSorting() {
		$model = $this->_model;
		$model::config(array('meta' => array('source' => 'ordered_docs', 'locked' => false)));

		$first = array('title' => 'First document',  'position' => 1);
		$second = array('title' => 'Second document', 'position' => 2);
		$third = array('title' => 'Third document',  'position' => 3);

		$model::create($third)->save();
		$model::create($first)->save();
		$model::create($second)->save();

		$result = $this->db->connection->queries;
		$createOpts = array(
			'validate' => true,
			'events' => 'create',
			'whitelist' => null,
			'callbacks' => true,
			'locked' => false,
			'safe' => false,
			'fsync' => false
		);
		$baseInsert = array(
			'type' => 'insert',
			'collection' => 'ordered_docs',
			'options' => $createOpts
		);

		$expected = array(
			$baseInsert + array('data' => array('_id' => $result[0]['data']['_id']) + $third),
			$baseInsert + array('data' => array('_id' => $result[1]['data']['_id']) + $first),
			$baseInsert + array('data' => array('_id' => $result[2]['data']['_id']) + $second)
		);
		$this->assertEqual($expected, $result);

		array_push($this->db->connection->results, new MockResult(array(
			'data' => array($first, $second, $third)
		)));
		$this->db->connection->queries = array();
		$documents = $model::all(array('order' => 'position'));

		$this->assertEqual($first['title'], $documents[0]->title);
		$this->assertEqual($second['title'], $documents[1]->title);
		$this->assertEqual($third['title'], $documents[2]->title);

		$expected = array(
			'type' => 'find',
			'collection' => 'ordered_docs',
			'conditions' => array(),
			'fields' => array()
		);
		$this->assertEqual($expected, array_pop($this->db->connection->queries));
		$result = $documents->result()->resource()->query['sort'];
		$this->assertEqual(array('position' => 1), $result);

		array_push($this->db->connection->results, new MockResult(array(
			'data' => array($first, $second, $third)
		)));
		$documents = $model::all(array('order' => array('position' => 'asc')));

		$this->assertEqual($first['title'], $documents[0]->title);
		$this->assertEqual($second['title'], $documents[1]->title);
		$this->assertEqual($third['title'], $documents[2]->title);

		$this->assertEqual($expected, array_pop($this->db->connection->queries));
		$result = $documents->result()->resource()->query['sort'];
		$this->assertEqual(array('position' => 1), $result);

		array_push($this->db->connection->results, new MockResult(array(
			'data' => array($third, $second, $first)
		)));
		$documents = $model::all(array('order' => array('position' => 'desc')));

		$this->assertEqual($third['title'], $documents[0]->title);
		$this->assertEqual($second['title'], $documents[1]->title);
		$this->assertEqual($first['title'], $documents[2]->title);

		$this->assertEqual($expected, array_pop($this->db->connection->queries));
		$result = $documents->result()->resource()->query['sort'];
		$this->assertEqual(array('position' => -1), $result);
	}

	public function testMongoIdPreservation() {
		$model = $this->_model;
		$model::config(array('meta' => array('locked' => false)));

		$post = $model::create(array('_id' => new MongoId(), 'title' => 'A post'));
		$post->save();
		$result = array_pop($this->db->connection->queries);
		$data = $result['data'];

		$this->assertEqual('A post', $data['title']);
		$this->assertTrue($data['_id'] instanceof MongoId);

		$post->sync();
		$post->title = 'An updated post';
		$post->save();

		$result = array_pop($this->db->connection->queries);
		$this->assertEqual(array('_id' => $post->_id), $result['conditions']);
		$this->assertEqual(array('$set' => array('title' => 'An updated post')), $result['update']);
	}

	public function testRelationshipGeneration() {
		$from = 'lithium\tests\mocks\data\MockComment';
		$to = 'lithium\tests\mocks\data\MockPost';

		$from::$connection = $this->db;
		$to::$connection = $this->db;

		$from::config(array(
			'schema' => new Schema(array('fields' => array('comment_id')))
		));
		$to::config(array('meta' => array('key' => '_id')));

		$result = $this->db->relationship($from, 'belongsTo', 'MockPost');
		$expected = array(
			'name' => 'MockPost',
			'type' => 'belongsTo',
			'key' => array('mockComment' => '_id'),
			'from' => $from,
			'link' => 'contained',
			'to'   => $to,
			'fields' => true,
			'fieldName' => 'mockPost',
			'constraints' => null,
			'init' => true
		);
		$this->assertEqual($expected, $result->data());
	}

	public function testCreateNoConnectionException() {
		$db = new MongoDb(array('host' => '__invalid__', 'autoConnect' => false));
		$this->expectException('Could not connect to the database.');
		$result = $db->create(null);
	}

	public function testReadNoConnectionException() {
		$db = new MongoDb(array('host' => '__invalid__', 'autoConnect' => false));
		$this->expectException('Could not connect to the database.');
		$result = $db->read(null);
	}

	public function testUpdateNoConnectionException() {
		$db = new MongoDb(array('host' => '__invalid__', 'autoConnect' => false));
		$this->expectException('Could not connect to the database.');
		$result = $db->update(null);
	}

	public function testDeleteNoConnectionException() {
		$db = new MongoDb(array('host' => '__invalid__', 'autoConnect' => false));
		$this->expectException('Could not connect to the database.');
		$result = $db->delete(null);
	}

	public function testSourcesNoConnectionException() {
		$db = new MongoDb(array('host' => null, 'autoConnect' => false));
		$this->expectException('Could not connect to the database.');
		$result = $db->sources(null);
	}

	public function testAtomicUpdate() {
		$model = $this->_model;
		$model::config(array('meta' => array('source' => 'posts')));
		$data = array('initial' => 'one', 'values' => 'two');

		$this->db->connection = new MockMongoConnection();
		$this->db->connection->results = array(true, true);

		$document = $model::create($data);
		$this->assertTrue($document->save());

		$result = array_shift($this->db->connection->queries);
		$expected = array(
			'type' => 'insert',
			'collection' => 'posts',
			'data' => array('initial' => 'one', 'values' => 'two', '_id' => $document->_id),
			'options' => array(
				'validate' => true, 'events' => 'create', 'whitelist' => null, 'callbacks' => true,
				'locked' => false, 'safe' => false, 'fsync' => false
			)
		);
		$this->assertEqual($expected, $result);

		$duplicate = $model::create(array('_id' => $document->_id), array('exists' => true));
		$duplicate->values = 'new';
		$this->assertTrue($duplicate->save());

		$result = array_shift($this->db->connection->queries);
		$expected = array(
			'type' => 'update',
			'collection' => 'posts',
			'conditions' => array('_id' => $document->_id),
			'update' => array('$set' => array('values' => 'new')),
			'options' => array(
				'validate' => true, 'events' => 'update', 'whitelist' => null,
				'callbacks' => true, 'locked' => false, 'upsert' => false, 'multiple' => true,
				'safe' => false, 'fsync' => false
			)
		);
		$this->assertEqual($expected, $result);

		array_push($this->db->connection->results, new MockResult(array('data' => array(
			array('_id' => $duplicate->_id, 'initial' => 'one', 'values' => 'new')
		))));

		$document = $model::find($duplicate->_id);
		$expected = array('_id' => (string) $duplicate->_id, 'initial' => 'one', 'values' => 'new');
		$this->assertEqual($expected, $document->data());

		$result = array_shift($this->db->connection->queries);
		$expected = array(
			'type' => 'find', 'collection' => 'posts', 'fields' => array(), 'conditions' => array(
				'_id' => $duplicate->_id
			)
		);
		$this->assertEqual($expected, $result);
	}

	/**
	 * Tests that the MongoDB adapter will not attempt to overwrite the _id field on document
	 * update.
	 */
	public function testPreserveId() {
		$model = $this->_model;
		$document = $model::create(array('_id' => 'custom'), array('exists' => true));

		array_push($this->db->connection->results, true);
		$this->assertTrue($document->save(array('_id' => 'custom2', 'foo' => 'bar')));

		$result = array_shift($this->db->connection->queries);
		$expected = array('$set' => array('foo' => 'bar'));
		$this->assertEqual($expected, $result['update']);
	}

	public function testCastingConditionsValues() {
		$query = new Query(array('schema' => new Schema(array('fields' => $this->_schema))));

		$conditions = array('_id' => new MongoId("4c8f86167675abfabdbe0300"));
		$result = $this->db->conditions($conditions, $query);
		$this->assertEqual($conditions, $result);

		$conditions = array('_id' => "4c8f86167675abfabdbe0300");
		$result = $this->db->conditions($conditions, $query);

		$this->assertEqual(array_keys($conditions), array_keys($result));
		$this->assertTrue($result['_id'] instanceof MongoId);
		$this->assertEqual($conditions['_id'], (string) $result['_id']);

		$conditions = array('_id' => array(
			"4c8f86167675abfabdbe0300", "4c8f86167675abfabdbf0300", "4c8f86167675abfabdc00300"
		));
		$result = $this->db->conditions($conditions, $query);
		$this->assertEqual(3, count($result['_id']['$in']));

		foreach (array(0, 1, 2) as $i) {
			$this->assertTrue($result['_id']['$in'][$i] instanceof MongoId);
		}

		$conditions = array('voters' => array('$all' => array(
			"4c8f86167675abfabdbf0300", "4c8f86167675abfabdc00300"
		)));
		$result = $this->db->conditions($conditions, $query);
		$this->assertEqual(2, count($result['voters']['$all']));
		$result = $result['voters']['$all'];

		foreach (array(0, 1) as $i) {
			$this->assertTrue($result[$i] instanceof MongoId);
			$this->assertEqual($conditions['voters']['$all'][$i], (string) $result[$i]);
		}

		$conditions = array('$or' => array(
			array('_id' => "4c8f86167675abfabdbf0300"),
			array('guid' => "4c8f86167675abfabdbf0300")
		));
		$result = $this->db->conditions($conditions, $query);
		$this->assertEqual(array('$or'), array_keys($result));
		$this->assertEqual(2, count($result['$or']));


		foreach (array('_id', 'guid') as $i => $key) {
			$this->assertTrue($result['$or'][$i][$key] instanceof MongoId);
			$this->assertEqual($conditions['$or'][$i][$key], (string) $result['$or'][$i][$key]);
		}
	}

	public function testMultiOperationConditions() {
		$conditions = array('loc' => array('$near' => array(50, 50), '$maxDistance' => 5));
		$result = $this->db->conditions($conditions, $this->query);
		$this->assertEqual($conditions, $result);
	}

	public function testCreateWithEmbeddedObjects() {
		$data = array(
			'_id' => new MongoId(),
			'created' => new MongoDate(strtotime('-1 hour')),
			'list' => array('foo', 'bar', 'baz')
		);
		$entity = new Document(compact('data') + array('exists' => false));
		$query = new Query(array('type' => 'create') + compact('entity'));
		$result = $query->export($this->db);
		$this->assertIdentical($data, $result['data']['data']);
	}

	public function testUpdateWithEmbeddedObjects() {
		$data = array(
			'_id' => new MongoId(),
			'created' => new MongoDate(strtotime('-1 hour')),
			'list' => array('foo', 'bar', 'baz')
		);
		$model = $this->_model;
		$fields = array('updated' => array('type' => 'MongoDate'));
		$schema = new Schema(compact('fields'));
		$entity = new Document(compact('data', 'schema', 'model') + array('exists' => true));
		$entity->updated = time();
		$entity->list[] = 'dib';

		$query = new Query(array('type' => 'update') + compact('entity'));
		$result = $query->export($this->db);
		$expected = array('_id', 'created', 'list', 'updated');
		$this->assertEqual($expected, array_keys($result['data']['update']));
		$this->assertTrue($result['data']['update']['updated'] instanceof MongoDate);
	}

	/**
	 * Assert that Mongo and the Mongo Exporter don't mangle manual geospatial queries.
	 */
	public function testGeoQueries() {
		$coords = array(84.13, 11.38);
		$coords2 = array_map(function($point) { return $point + 5; }, $coords);
		$conditions = array('location' => array('$near' => $coords));

		$query = new Query(compact('conditions') + array('model' => $this->_model));
		$result = $query->export($this->db);
		$this->assertEqual($result['conditions'], $conditions);

		$conditions = array('location' => array(
			'$within' => array('$box' => array($coords2, $coords))
		));
		$query = new Query(compact('conditions') + array('model' => $this->_model));
		$result = $query->export($this->db);
		$this->assertEqual($conditions, $result['conditions']);
	}

	public function testSchemaCallback() {
		$schema = array('_id' => array('type' => 'id'), 'created' => array('type' => 'date'));
		$db = new MongoDb(array('autoConnect' => false, 'schema' => function() use ($schema) {
			return $schema;
		}));
		$this->assertEqual($schema, $db->describe(null)->fields());
	}

	public function testSetReadPreference() {
		$prefs = array(
			"SECONDARY",
			array('dc' => 'east', 'use' => 'reporting')
		);
		$db = new MongoDb(array(
			'readPreference' => $prefs,
			'classes' => array(
				'server' => 'lithium\tests\mocks\core\MockCallable'
			)
		));

		$result = $db->server->call;
		$this->assertEqual('setReadPreference', $result['method']);
		$this->assertEqual($prefs, $result['params']);
	}

	public function testDefaultSafeOptions() {
		$this->db = new MongoDb($this->_testConfig + array('safe' => true));
		$this->db->server = new MockMongoConnection();
		$this->db->connection = new MockCallable();
		$this->db->connection->custom = new MockCallable();
		$this->db->server->connected = true;

		$query = new Query(array('type' => 'read', 'source' => 'custom'));
		$this->db->create($query);
		$result = $this->db->connection->custom->call;
		$expected = array(null, array('safe' => true, 'fsync' => false));
		$this->assertEqual('insert', $result['method']);
		$this->assertEqual($expected, $result['params']);

		$query = new Query(array('type' => 'read', 'source' => 'custom'));
		$this->db->update($query);
		$result = $this->db->connection->custom->call;
		$expected = array('upsert' => false, 'multiple' => true, 'safe' => true, 'fsync' => false);
		$this->assertEqual('update', $result['method']);
		$this->assertEqual($expected, $result['params'][2]);

		$query = new Query(array('type' => 'read', 'source' => 'custom'));
		$this->db->delete($query);
		$result = $this->db->connection->custom->call;
		$expected = array('justOne' => false, 'safe' => true, 'fsync' => false);
		$this->assertEqual('remove', $result['method']);
		$this->assertEqual($expected, $result['params'][1]);

		$this->db = new MongoDb($this->_testConfig + array('safe' => false));
		$this->db->server = new MockMongoConnection();
		$this->db->connection = new MockCallable();
		$this->db->connection->custom = new MockCallable();
		$this->db->server->connected = true;

		$query = new Query(array('type' => 'read', 'source' => 'custom'));
		$this->db->create($query);
		$result = $this->db->connection->custom->call;
		$expected = array(null, array('safe' => false, 'fsync' => false));
		$this->assertEqual('insert', $result['method']);
		$this->assertEqual($expected, $result['params']);

		$query = new Query(array('type' => 'read', 'source' => 'custom'));
		$this->db->update($query);
		$result = $this->db->connection->custom->call;
		$expected = array('upsert' => false, 'multiple' => true, 'safe' => false, 'fsync' => false);
		$this->assertEqual('update', $result['method']);
		$this->assertEqual($expected, $result['params'][2]);

		$query = new Query(array('type' => 'read', 'source' => 'custom'));
		$this->db->delete($query);
		$result = $this->db->connection->custom->call;
		$expected = array('justOne' => false, 'safe' => false, 'fsync' => false);
		$this->assertEqual('remove', $result['method']);
		$this->assertEqual($expected, $result['params'][1]);
	}

	public function testGridFsCRUDWithDefaultPrefix() {
		$model = $this->_model;
		$source = 'fs.files';
		$data = array('filename' => 'lithium', 'file' => 'some_datas');

		$model::config(array('meta' => array('source' => $source, 'locked' => false)));
		$this->assertIdentical(true, $model::create()->save($data));
		$this->assertIdentical('fs', $this->db->connection->gridFsPrefix);
		$this->db->connection->gridFsPrefix = null;

		$model::config(array('meta' => array('source' => $source, 'locked' => false)));
		$this->db->connection->results = array(new MockResult(array('data' => $data)));
		$this->assertTrue($model::find('all'));
		$this->assertIdentical('fs', $this->db->connection->gridFsPrefix);
		$this->db->connection->gridFsPrefix = null;

		$model::create($data + array('_id' => new MongoId), array('exists' => true))->delete();
		$this->assertIdentical('fs', $this->db->connection->gridFsPrefix);
		$this->db->connection->gridFsPrefix = null;

	}

	public function testGridFsCreateWithCustomPrefix() {
		$model = $this->_model;
		$data = array('filename' => 'lithium', 'file' => 'some_datas');

		$db = new MongoDb($this->_testConfig + array('gridPrefix' => 'custom'));
		$db->server = new MockMongoConnection();
		$db->connection = new MockMongoConnection();
		$db->server->connected = true;
		$model::$connection = $db;

		$model::config(array('meta' => array('source' => 'fs.files', 'locked' => false)));
		$this->assertIdentical(false, $model::create()->save($data));
		$this->assertIdentical(null, $db->connection->gridFsPrefix);

		$model::config(array('meta' => array('source' => 'custom.files', 'locked' => false)));
		$this->assertIdentical(true, $model::create()->save($data));
		$this->assertIdentical('custom', $db->connection->gridFsPrefix);
	}

	public function testGridFsReadWithCustomPrefix() {
		$model = $this->_model;
		$data = array('filename' => 'lithium', 'file' => 'some_datas');
		$result = new MockResult(array('data' => array(
			array('filename' => 'lithium', 'file' => 'some_datas')
		)));

		$db = new MongoDb($this->_testConfig + array('gridPrefix' => 'custom'));
		$db->server = new MockMongoConnection();
		$db->connection = new MockMongoConnection();
		$db->server->connected = true;
		$model::$connection = $db;

		$model::config(array('meta' => array('source' => 'fs.files', 'locked' => false)));
		$db->connection->results = array($result);
		$this->assertTrue($model::find('all'));
		$this->assertIdentical(null, $db->connection->gridFsPrefix);

		$model::config(array('meta' => array('source' => 'custom.files', 'locked' => false)));
		$db->connection->results = array($result);
		$this->assertTrue($model::find('all'));
		$this->assertIdentical('custom', $db->connection->gridFsPrefix);
	}

	public function testGridFsDeleteWithCustomPrefix() {
		$model = $this->_model;
		$data = array('_id' => new MongoId);

		$db = new MongoDb($this->_testConfig + array('gridPrefix' => 'custom'));
		$db->server = new MockMongoConnection();
		$db->connection = new MockMongoConnection();
		$db->server->connected = true;
		$model::$connection = $db;

		$model::config(array('meta' => array('source' => 'fs.files', 'locked' => false)));
		$model::create($data, array('exists' => true))->delete();
		$this->assertIdentical(null, $db->connection->gridFsPrefix);

		$model::config(array('meta' => array('source' => 'custom.files', 'locked' => false)));
		$model::create($data, array('exists' => true))->delete();
		$this->assertIdentical('custom', $db->connection->gridFsPrefix);
	}

	public function testRespondsToParentCall() {
		$db = new MongoDb($this->_testConfig);
		$this->assertTrue($db->respondsTo('applyFilter'));
		$this->assertFalse($db->respondsTo('fooBarBaz'));
	}

	public function testRespondsToWithNoServer() {
		$db = new MongoDb($this->_testConfig);
		$this->assertFalse($db->respondsTo('listDBs'));
		$this->assertFalse($db->respondsTo('foobarbaz'));
	}

	public function testRespondsToWithServer() {
		$db = new MongoDb($this->_testConfig);
		$db->server = new MockMongoConnection();
		$this->assertTrue($db->respondsTo('listDBs'));
		$this->assertFalse($db->respondsTo('foobarbaz'));
	}

}

?>