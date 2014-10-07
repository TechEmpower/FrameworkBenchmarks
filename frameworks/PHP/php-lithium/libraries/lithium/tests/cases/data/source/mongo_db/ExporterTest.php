<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\data\source\mongo_db;

use MongoId;
use MongoDate;
use lithium\data\source\MongoDb;
use lithium\data\entity\Document;
use lithium\data\collection\DocumentSet;
use lithium\data\source\mongo_db\Schema;
use lithium\data\source\mongo_db\Exporter;
use lithium\tests\mocks\data\source\mongo_db\MockResult;

class ExporterTest extends \lithium\test\Unit {

	protected $_model = 'lithium\tests\mocks\data\source\MockMongoPost';

	protected $_schema = array(
		'_id' => array('type' => 'id'),
		'guid' => array('type' => 'id'),
		'title' => array('type' => 'string'),
		'tags' => array('type' => 'string', 'array' => true),
		'comments' => array('type' => 'MongoId'),
		'accounts' => array('type' => 'object', 'array' => true),
		'accounts._id' => array('type' => 'id'),
		'accounts.name' => array('type' => 'string'),
		'accounts.created' => array('type' => 'date'),
		'authors' => array('type' => 'MongoId', 'array' => true),
		'created' => array('type' => 'MongoDate'),
		'modified' => array('type' => 'datetime'),
		'voters' => array('type' => 'id', 'array' => true),
		'rank_count' => array('type' => 'integer', 'default' => 0),
		'rank' => array('type' => 'float', 'default' => 0.0),
		'notifications.foo' => array('type' => 'boolean'),
		'notifications.bar' => array('type' => 'boolean'),
		'notifications.baz' => array('type' => 'boolean')
	);

	protected $_handlers = array();

	public function skip() {
		$this->skipIf(!MongoDb::enabled(), 'MongoDb is not enabled');
	}

	public function setUp() {
		$this->_handlers = array(
			'id' => function($v) {
				return is_string($v) && preg_match('/^[0-9a-f]{24}$/', $v) ? new MongoId($v) : $v;
			},
			'date' => function($v) {
				$v = is_numeric($v) ? intval($v) : strtotime($v);
				return !$v ? new MongoDate() : new MongoDate($v);
			},
			'regex'   => function($v) { return new MongoRegex($v); },
			'integer' => function($v) { return (integer) $v; },
			'float'   => function($v) { return (float) $v; },
			'boolean' => function($v) { return (boolean) $v; },
			'code'    => function($v) { return new MongoCode($v); },
			'binary'  => function($v) { return new MongoBinData($v); }
		);
		$model = $this->_model;
		$model::$connection = new MongoDb(array('autoConnect' => false));

		$model::schema(false);
		$model::schema($this->_schema);
	}

	public function testInvalid() {
		$this->assertNull(Exporter::get(null, null));
	}

	public function testCreateWithFixedData() {
		$time = time();
		$doc = new Document(array('exists' => false, 'data' => array(
			'_id' => new MongoId(),
			'created' => new MongoDate($time),
			'numbers' => new DocumentSet(array('data' => array(7, 8, 9))),
			'objects' => new DocumentSet(array('data' => array(
				new Document(array('data' => array('foo' => 'bar'))),
				new Document(array('data' => array('baz' => 'dib')))
			))),
			'deeply' => new Document(array('data' => array('nested' => 'object')))
		)));
		$this->assertEqual('object', $doc->deeply->nested);
		$this->assertTrue($doc->_id instanceof MongoId);

		$result = Exporter::get('create', $doc->export());
		$this->assertTrue($result['create']['_id'] instanceof MongoId);
		$this->assertTrue($result['create']['created'] instanceof MongoDate);
		$this->assertIdentical($time, $result['create']['created']->sec);

		$this->assertIdentical(array(7, 8, 9), $result['create']['numbers']);
		$expected = array(array('foo' => 'bar'), array('baz' => 'dib'));
		$this->assertIdentical($expected, $result['create']['objects']);
		$this->assertIdentical(array('nested' => 'object'), $result['create']['deeply']);
	}

	public function testCreateWithChangedData() {
		$doc = new Document(array('exists' => false, 'data' => array(
			'numbers' => new DocumentSet(array('data' => array(7, 8, 9))),
			'objects' => new DocumentSet(array('data' => array(
				new Document(array('data' => array('foo' => 'bar'))),
				new Document(array('data' => array('baz' => 'dib')))
			))),
			'deeply' => new Document(array('data' => array('nested' => 'object')))
		)));
		$doc->numbers[] = 10;
		$doc->deeply->nested2 = 'object2';
		$doc->objects[1]->dib = 'gir';

		$expected = array(
			'numbers' => array(7, 8, 9, 10),
			'objects' => array(array('foo' => 'bar'), array('baz' => 'dib', 'dib' => 'gir')),
			'deeply' => array('nested' => 'object', 'nested2' => 'object2')
		);
		$result = Exporter::get('create', $doc->export());
		$this->assertEqual(array('create'), array_keys($result));
		$this->assertEqual($expected, $result['create']);
	}

	public function testUpdateWithNoChanges() {
		$doc = new Document(array('exists' => true, 'data' => array(
			'numbers' => new DocumentSet(array('exists' => true, 'data' => array(7, 8, 9))),
			'objects' => new DocumentSet(array('exists' => true, 'data' => array(
				new Document(array('exists' => true, 'data' => array('foo' => 'bar'))),
				new Document(array('exists' => true, 'data' => array('baz' => 'dib')))
			))),
			'deeply' => new Document(array('exists' => true, 'data' => array('nested' => 'object')))
		)));
		$this->assertFalse(Exporter::get('update', $doc->export()));
	}

	public function testUpdateFromResourceLoading() {
		$resource = new MockResult();
		$doc = new DocumentSet(array('model' => $this->_model, 'result' => $resource));
		$this->assertFalse(Exporter::get('update', $doc->export()));
		$this->assertEqual('dib', $doc['6c8f86167675abfabdbf0302']->title);

		$doc['6c8f86167675abfabdbf0302']->title = 'bob';
		$this->assertEqual('6c8f86167675abfabdbf0302', $doc['6c8f86167675abfabdbf0302']->_id);
		$this->assertEqual('bob', $doc['6c8f86167675abfabdbf0302']->title);

		$doc['4c8f86167675abfabdbf0300']->title = 'bill';
		$this->assertEqual('4c8f86167675abfabdbf0300', $doc['4c8f86167675abfabdbf0300']->_id);
		$this->assertEqual('bill', $doc['4c8f86167675abfabdbf0300']->title);

		$expected = Exporter::get('update', $doc->export());
		$this->assertTrue(Exporter::get('update', $doc->export()));
		$this->assertEqual(2, count($expected['update']));
	}

	public function testUpdateWithSubObjects() {
		$model = $this->_model;
		$exists = true;
		$model::config(array('meta' => array('key' => '_id')));
		$schema = new Schema(array('fields' => array(
			'forceArray' => array('type' => 'string', 'array' => true),
			'array' => array('type' => 'string', 'array' => true),
			'dictionary' => array('type' => 'string', 'array' => true),
			'numbers' => array('type' => 'integer', 'array' => true),
			'objects' => array('type' => 'object', 'array' => true),
			'deeply' => array('type' => 'object', 'array' => true),
			'foo' => array('type' => 'string')
		)));
		$config = compact('model', 'schema', 'exists');

		$doc = new Document($config + array('data' => array(
			'numbers' => new DocumentSet($config + array(
				'data' => array(7, 8, 9), 'pathKey' => 'numbers'
			)),
			'objects' => new DocumentSet($config + array('pathKey' => 'objects', 'data' => array(
				new Document($config + array('data' => array('foo' => 'bar'))),
				new Document($config + array('data' => array('foo' => 'baz')))
			))),
			'deeply' => new Document($config + array('pathKey' => 'deeply', 'data' => array(
				'nested' => 'object'
			))),
			'foo' => 'bar'
		)));
		$doc->dictionary[] = 'A Word';
		$doc->forceArray = 'Word';
		$doc->array = array('one');
		$doc->field = 'value';
		$doc->objects[1]->foo = 'dib';
		$doc->objects[] = array('foo' => 'diz');
		$doc->deeply->nested = 'foo';
		$doc->deeply->nestedAgain = 'bar';
		$doc->array = array('one');
		$doc->newObject = new Document(array(
			'exists' => false, 'data' => array('subField' => 'subValue')
		));
		$doc->newObjects = array(
			array('test' => 'one', 'another' => 'two'),
			array('three' => 'four')
		);
		$this->assertEqual('foo', $doc->deeply->nested);
		$this->assertEqual('subValue', $doc->newObject->subField);

		$doc->numbers = array(8, 9);
		$doc->numbers[] = 10;
		$doc->numbers->append(11);

		$export = $doc->export();

		$result = Exporter::get('update', $doc->export());
		$expected = array(
			'array' => array('one'),
			'dictionary' => array('A Word'),
			'forceArray' => array('Word'),
			'numbers' => array(8, 9, 10, 11),
			'newObject' => array('subField' => 'subValue'),
			'newObjects' => array(
				array('test' => 'one', 'another' => 'two'),
				array('three' => 'four')
			),
			'field' => 'value',
			'deeply.nested' => 'foo',
			'deeply.nestedAgain' => 'bar',
			'array' => array('one'),
			'objects.1.foo' => 'dib',
			'objects.2' => array('foo' => 'diz')
		);
		$this->assertEqual($expected, $result['update']);

		$doc->objects[] = array('foo' => 'dob');

		$exist = $doc->objects->find(
			function ($data) { return (strcmp($data->foo, 'dob') === 0); },
			array('collect' => false)
		);
		$this->assertTrue(!empty($exist));
	}

	public function testFieldRemoval() {
		$doc = new Document(array('exists' => true, 'data' => array(
			'numbers' => new DocumentSet(array('data' => array(7, 8, 9))),
			'deeply' => new Document(array(
				'pathKey' => 'deeply', 'exists' => true, 'data' => array('nested' => 'object')
			)),
			'foo' => 'bar'
		)));

		unset($doc->numbers);
		$result = Exporter::get('update', $doc->export());
		$this->assertEqual(array('numbers' => true), $result['remove']);

		$doc->set(array('flagged' => true, 'foo' => 'baz', 'bar' => 'dib'));
		unset($doc->foo, $doc->flagged, $doc->numbers, $doc->deeply->nested);
		$result = Exporter::get('update', $doc->export());
		$expected = array('foo' => true, 'deeply.nested' => true, 'numbers' => true);
		$this->assertEqual($expected, $result['remove']);
		$this->assertEqual(array('bar' => 'dib'), $result['update']);
	}

	/**
	 * Tests that when an existing object is attached as a value of another existing object, the
	 * whole sub-object is re-written to the new value.
	 */
	public function testAppendExistingObjects() {
		$doc = new Document(array('exists' => true, 'data' => array(
			'deeply' => new Document(array(
				'pathKey' => 'deeply', 'exists' => true, 'data' => array('nested' => 'object')
			)),
			'foo' => 'bar'
		)));
		$append = new Document(array('exists' => true, 'data' => array('foo' => 'bar')));

		$doc->deeply = $append;
		$result = Exporter::get('update', $doc->export());
		$expected = array('update' => array('deeply' => array('foo' => 'bar')));
		$this->assertEqual($expected, $result);

		$expected = array('$set' => array('deeply' => array('foo' => 'bar')));
		$this->assertEqual($expected, Exporter::toCommand($result));

		$doc->sync();
		$doc->append2 = new Document(array('exists' => false, 'data' => array('foo' => 'bar')));
		$expected = array('update' => array('append2' => array('foo' => 'bar')));
		$this->assertEqual($expected, Exporter::get('update', $doc->export()));
		$doc->sync();

		$this->assertFalse(Exporter::get('update', $doc->export()));
		$doc->append2->foo = 'baz';
		$doc->append2->bar = 'dib';
		$doc->deeply->nested = true;

		$expected = array('update' => array(
			'append2.foo' => 'baz', 'append2.bar' => 'dib', 'deeply.nested' => true
		));
		$this->assertEqual($expected, Exporter::get('update', $doc->export()));
	}

	public function testNestedObjectCasting() {
		$model = $this->_model;
		$data = array('notifications' => array('foo' => '', 'bar' => '1', 'baz' => 0, 'dib' => 42));
		$result = $model::schema()->cast(null, null, $data, compact('model'));

		$this->assertIdentical(false, $result['notifications']->foo);
		$this->assertIdentical(true, $result['notifications']->bar);
		$this->assertIdentical(false, $result['notifications']->baz);
		$this->assertIdentical(42, $result['notifications']->dib);
	}

	/**
	 * Tests handling type values based on specified schema settings.
	 *
	 * @return void
	 */
	public function testTypeCasting() {
		$time = time();
		$data = array(
			'_id' => '4c8f86167675abfabd970300',
			'title' => 'Foo',
			'tags' => 'test',
			'comments' => array(
				"4c8f86167675abfabdbe0300", "4c8f86167675abfabdbf0300", "4c8f86167675abfabdc00300"
			),
			'empty_array' => array(),
			'authors' => '4c8f86167675abfabdb00300',
			'created' => $time,
			'modified' => date('Y-m-d H:i:s', $time),
			'rank_count' => '45',
			'rank' => '3.45688'
		);
		$model = $this->_model;
		$handlers = $this->_handlers;
		$options = compact('model', 'handlers');
		$schema = new Schema(array('fields' => $this->_schema));
		$result = $schema->cast(null, null, $data, $options);
		$this->assertEqual(array_keys($data), array_keys($result->data()));
		$this->assertTrue($result->_id instanceof MongoId);
		$this->assertEqual('4c8f86167675abfabd970300', (string) $result->_id);

		$this->assertTrue($result->comments instanceof DocumentSet);
		$this->assertEqual(3, count($result['comments']));

		$this->assertTrue($result->comments[0] instanceof MongoId);
		$this->assertTrue($result->comments[1] instanceof MongoId);
		$this->assertTrue($result->comments[2] instanceof MongoId);
		$this->assertEqual('4c8f86167675abfabdbe0300', (string) $result->comments[0]);
		$this->assertEqual('4c8f86167675abfabdbf0300', (string) $result->comments[1]);
		$this->assertEqual('4c8f86167675abfabdc00300', (string) $result->comments[2]);

		$this->assertEqual($data['comments'], $result->comments->data());
		$this->assertEqual(array('test'), $result->tags->data());
		$this->assertEqual(array('4c8f86167675abfabdb00300'), $result->authors->data());
		$this->assertTrue($result->authors[0] instanceof MongoId);

		$this->assertTrue($result->modified instanceof MongoDate);
		$this->assertTrue($result->created instanceof MongoDate);
		$this->assertTrue($result->created->sec > 0);

		$this->assertTrue($result->empty_array instanceof DocumentSet);

		$this->assertEqual($time, $result->modified->sec);
		$this->assertEqual($time, $result->created->sec);

		$this->assertIdentical(45, $result->rank_count);
		$this->assertIdentical(3.45688, $result->rank);
	}

	/**
	 * Tests handling type values of subdocument arrays based on specified schema settings.
	 *
	 * @return void
	 */
	public function testTypeCastingSubObjectArrays() {
		$time = time();
		$data = array(
			'_id' => '4c8f86167675abfabd970300',
			'accounts' => array(array(
				'_id' => "4fb6e2dd3e91581fe6e75736",
				'name' => 'Foo',
				'created' => $time
			),array(
				'_id' => "4fb6e2df3e91581fe6e75737",
				'name' => 'Bar',
				'created' => $time
			))
		);
		$model = $this->_model;
		$handlers = $this->_handlers;
		$options = compact('model', 'handlers');
		$schema = new Schema(array('fields' => $this->_schema));
		$result = $schema->cast(null, null, $data, $options);

		$this->assertEqual(array_keys($data), array_keys($result->data()));
		$this->assertTrue($result->_id instanceof MongoId);
		$this->assertEqual('4c8f86167675abfabd970300', (string) $result->_id);

		$this->assertTrue($result->accounts instanceof DocumentSet);
		$this->assertEqual(2, count($result->accounts));

		$id1 = '4fb6e2dd3e91581fe6e75736';
		$id2 = '4fb6e2df3e91581fe6e75737';
		$this->assertTrue($result->accounts[$id1]['_id'] instanceof MongoId);
		$this->assertEqual($id1, (string) $result->accounts[$id1]['_id']);
		$this->assertTrue($result->accounts[$id2]['_id'] instanceof MongoId);
		$this->assertEqual($id2, (string) $result->accounts[$id2]['_id']);

		$this->assertTrue($result->accounts[$id1]['created'] instanceof MongoDate);
		$this->assertTrue($result->accounts[$id1]['created']->sec > 0);
		$this->assertTrue($result->accounts[$id2]['created'] instanceof MongoDate);
		$this->assertTrue($result->accounts[$id2]['created']->sec > 0);
	}

	public function testWithArraySchema() {
		$model = $this->_model;
		$model::schema(array(
			'_id' => array('type' => 'id'),
			'list' => array('type' => 'string', 'array' => true),
			'obj.foo' => array('type' => 'string'),
			'obj.bar' => array('type' => 'string')
		));
		$doc = new Document(compact('model'));
		$doc->list[] = array('foo' => '!!', 'bar' => '??');

		$data = array('list' => array(array('foo' => '!!', 'bar' => '??')));
		$this->assertEqual($data, $doc->data());

		$result = Exporter::get('create', $doc->export());
		$this->assertEqual($data, $result['create']);

		$result = Exporter::get('update', $doc->export());
		$this->assertEqual($data, $result['update']);

		$doc = new Document(compact('model'));
		$doc->list = array();
		$doc->list[] = array('foo' => '!!', 'bar' => '??');

		$data = array('list' => array(array('foo' => '!!', 'bar' => '??')));
		$this->assertEqual($data, $doc->data());

		$result = Exporter::get('create', $doc->export());
		$this->assertEqual($result['create'], $data);

		$result = Exporter::get('update', $doc->export());
		$this->assertEqual($result['update'], $data);
	}

	/**
	 * Test that sub-objects are properly casted on creating a new `Document`.
	 */
	public function testSubObjectCastingOnSave() {
		$model = $this->_model;
		$model::schema(array(
			'_id' => array('type' => 'id'),
			'sub.foo' => array('type' => 'boolean'),
			'bar' => array('type' => 'boolean')
		));
		$data = array('sub' => array('foo' => 0), 'bar' => 1);
		$doc = new Document(compact('data', 'model'));

		$this->assertIdentical(true, $doc->bar);
		$this->assertIdentical(false, $doc->sub->foo);

		$data = array('sub.foo' => '1', 'bar' => '0');
		$doc = new Document(compact('data', 'model', 'schema'));

		$this->assertIdentical(false, $doc->bar);
		$this->assertIdentical(true, $doc->sub->foo);
	}

	/**
	 * Tests that a nested key on a previously saved document gets updated properly.
	 */
	public function testExistingNestedKeyOverwrite() {
		$doc = new Document(array('model' => $this->_model));
		$doc->{'this.that'} = 'value1';
		$this->assertEqual(array('this' => array('that' => 'value1')), $doc->data());

		$result = Exporter::get('create', $doc->export());
		$this->assertEqual(array('create' => array('this' => array('that' => 'value1'))), $result);

		$doc->sync();
		$doc->{'this.that'} = 'value2';
		$this->assertEqual(array('this' => array('that' => 'value2')), $doc->data());

		$result = Exporter::get('update', $doc->export());
		$this->assertEqual(array('update' => array('this.that' => 'value2')), $result);
	}

	public function testUpdatingArraysAndExporting() {
		$new = new Document(array('data' => array('name' => 'Acme, Inc.', 'active' => true)));

		$expected = array('name' => 'Acme, Inc.', 'active' => true);
		$result = $new->data();
		$this->assertEqual($expected, $result);

		$new->foo = new DocumentSet(array('data' => array('bar')));
		$expected = array('name' => 'Acme, Inc.', 'active' => true, 'foo' => array('bar'));
		$result = $new->data();
		$this->assertEqual($expected, $result);

		$expected = 'bar';
		$result = $new->foo[0];
		$this->assertEqual($expected, $result);

		$new->foo[1] = 'baz';

		$expected = 'baz';
		$result = $new->data();
		$this->assertEqual($expected, $result['foo'][1]);
	}

	/**
	 * Tests that arrays of nested objects can be appended to and will be updated using the proper
	 * MongoDB operators.
	 */
	public function testAppendingNestedObjectArray() {
		$model = $this->_model;
		$model::schema(false);
		$model::schema(array(
			'accounts' => array('type' => 'object', 'array' => true),
			'accounts.name' => array('type' => 'string')
		));
		$doc = new Document(compact('model'));
		$this->assertEqual(array(), $doc->accounts->data());
		$doc->sync();

		$data = array('name' => 'New account');
		$doc->accounts[] = new Document(compact('data'));

		$result = Exporter::get('update', $doc->export());
		$expected = array('update' => array('accounts.0' => $data));
		$this->assertEqual($expected, $result);

		$result = Exporter::toCommand($result);
		$expected = array('$set' => array('accounts.0' => $data));
		$this->assertEqual($expected, $result);
	}

	/**
	 * @todo Implement me.
	 */
	public function testCreateWithWhitelist() {
	}

	/**
	 * Tests the casting of MongoIds in nested arrays.
	 */
	public function testNestedArrayMongoIdCasting() {

		$articleOneId = new MongoId();
		$bookOneId = new MongoId();
		$bookTwoId = new MongoId();
		$data = array(
			'_id' => '4c8f86167675abfabd970300',
			'title' => 'Foo',
			'similar_text' => array(
				'articles' => array(
					$articleOneId
				),
				'books' => array(
					$bookOneId,
					$bookTwoId
				),
				'magazines' => array(
					"4fdfb4327a959c4f76000006",
					"4e95f6e098ef47722d000001"
				))
		);

		$model = $this->_model;
		$handlers = $this->_handlers;
		$options = compact('model', 'handlers');

		$schema = new Schema(array('fields' => array(
			'_id' => array('type' => 'MongoId'),
			'title' => array('type' => 'text'),
			'similar_text' => array('type' => 'array'),
			'similar_text.articles' => array('type' => 'MongoId', 'array' => true),
			'similar_text.books' => array('type' => 'MongoId', 'array' => true),
			'similar_text.magazines' => array('type' => 'MongoId', 'array' => true)
		)));
		$result = $schema->cast(null, null, $data, $options);
		$this->assertTrue($result['similar_text']['articles'][0] instanceof MongoId);
		$this->assertTrue($result['similar_text']['books'][0] instanceof MongoId);
		$this->assertTrue($result['similar_text']['books'][1] instanceof MongoId);
		$this->assertTrue($result['similar_text']['magazines'][0] instanceof MongoId);
		$this->assertTrue($result['similar_text']['magazines'][1] instanceof MongoId);
	}

	/**
	 * Tests that updating arrays of `MongoId`s correctly preserves their type.
	 */
	public function testUpdatingMongoIdArray() {
		$schema = new Schema(array('fields' => array(
			'list' => array('type' => 'id', 'array' => true)
		)));

		$doc = new Document(array('exists' => true, 'data' => array(
			'list' => array(new MongoId(), new MongoId(), new MongoId())
		)));
		$this->assertEqual(array(), Exporter::get('update', $doc->export()));

		$doc->list[] = new MongoId();
		$doc->list[] = new MongoId();
		$result = Exporter::get('update', $doc->export());

		$this->assertEqual(1, count($result));
		$this->assertEqual(1, count($result['update']));
		$this->assertEqual(5, count($result['update']['list']));

		for ($i = 0; $i < 5; $i++) {
			$this->assertTrue($result['update']['list'][$i] instanceof MongoId);
		}

		$doc = new Document(array('exists' => true, 'data' => array(
			'list' => array(new MongoId(), new MongoId(), new MongoId())
		)));
		$doc->list = array(new MongoId(), new MongoId(), new MongoId());
		$result = Exporter::get('update', $doc->export());

		$this->assertEqual(1, count($result));
		$this->assertEqual(1, count($result['update']));
		$this->assertEqual(3, count($result['update']['list']));

		for ($i = 0; $i < 3; $i++) {
			$this->assertTrue($result['update']['list'][$i] instanceof MongoId);
		}
	}

	public function testToData() {
		$data = array(
			array(
			'_id' => '4c8f86167675abfabd970300',
			'accounts' => array(array(
				'_id' => "4fb6e2dd3e91581fe6e75736",
				'name' => 'Foo1'
			),array(
				'_id' => "4fb6e2df3e91581fe6e75737",
				'name' => 'Bar1'
			))),
			array(
			'_id' => '4c8f86167675abfabd970301',
			'accounts' => array(array(
				'_id' => "4fb6e2dd3e91581fe6e75738",
				'name' => 'Foo2'
			),array(
				'_id' => "4fb6e2df3e91581fe6e75739",
				'name' => 'Bar2'
			)))
		);

		$model = $this->_model;
		$handlers = $this->_handlers;
		$options = compact('model', 'handlers');
		$schema = new Schema(array('fields' => $this->_schema));
		$set = $schema->cast(null, null, $data, $options);

		$result = $set->data();
		$accounts = $result['4c8f86167675abfabd970300']['accounts'];
		$this->assertEqual('Foo1', $accounts[0]['name']);
		$this->assertEqual('Bar1', $accounts[1]['name']);
		$accounts = $result['4c8f86167675abfabd970301']['accounts'];
		$this->assertEqual('Foo2', $accounts[0]['name']);
		$this->assertEqual('Bar2', $accounts[1]['name']);

		$result = $set->to('array', array('indexed' => false));
		$accounts = $result[0]['accounts'];
		$this->assertEqual('Foo1', $accounts[0]['name']);
		$this->assertEqual('Bar1', $accounts[1]['name']);
		$accounts = $result[1]['accounts'];
		$this->assertEqual('Foo2', $accounts[0]['name']);
		$this->assertEqual('Bar2', $accounts[1]['name']);

		$result = $set->to('array', array('indexed' => true));
		$accounts = $result['4c8f86167675abfabd970300']['accounts'];
		$this->assertEqual('Foo1', $accounts['4fb6e2dd3e91581fe6e75736']['name']);
		$this->assertEqual('Bar1', $accounts['4fb6e2df3e91581fe6e75737']['name']);
		$accounts = $result['4c8f86167675abfabd970301']['accounts'];
		$this->assertEqual('Foo2', $accounts['4fb6e2dd3e91581fe6e75738']['name']);
		$this->assertEqual('Bar2', $accounts['4fb6e2df3e91581fe6e75739']['name']);

		$result = $set->to('array');
		$accounts = $result['4c8f86167675abfabd970300']['accounts'];
		$this->assertEqual('Foo1', $accounts['4fb6e2dd3e91581fe6e75736']['name']);
		$this->assertEqual('Bar1', $accounts['4fb6e2df3e91581fe6e75737']['name']);
		$accounts = $result['4c8f86167675abfabd970301']['accounts'];
		$this->assertEqual('Foo2', $accounts['4fb6e2dd3e91581fe6e75738']['name']);
		$this->assertEqual('Bar2', $accounts['4fb6e2df3e91581fe6e75739']['name']);
	}

	public function testIndexesOnExportingDocumentSet() {
		$schema = new Schema(array('fields' => array(
			'_id' => array('type' => 'id'),
			'accounts' => array('type' => 'object', 'array' => true),
			'accounts._id' => array('type' => 'id'),
			'accounts.name' => array('type' => 'string')
		)));

		$data = array(
			array(
			'_id' => '4c8f86167675abfabd970300',
			'accounts' => array(array(
				'_id' => "4fb6e2dd3e91581fe6e75736",
				'name' => 'Foo1'
			),array(
				'_id' => "4fb6e2df3e91581fe6e75737",
				'name' => 'Bar1'
			))),
			array(
			'_id' => '4c8f86167675abfabd970301',
			'accounts' => array(array(
				'_id' => "4fb6e2dd3e91581fe6e75738",
				'name' => 'Foo2'
			),array(
				'_id' => "4fb6e2df3e91581fe6e75739",
				'name' => 'Bar2'
			)))
		);

		$model = $this->_model;

		$array = new DocumentSet(compact('model', 'schema', 'data'));
		$this->assertTrue($array['4c8f86167675abfabd970300']->accounts instanceof DocumentSet);
		$this->assertTrue($array['4c8f86167675abfabd970301']->accounts instanceof DocumentSet);

		$result = Exporter::get('create', $array->export());
		$this->assertTrue(isset($result['create'][0]));
		$this->assertTrue(isset($result['create'][1]));
		$this->assertFalse(isset($result['create']['4c8f86167675abfabd970300']));
		$this->assertFalse(isset($result['create']['4c8f86167675abfabd970301']));
		$this->assertTrue(isset($result['create'][0]['accounts'][0]));
		$this->assertTrue(isset($result['create'][0]['accounts'][1]));
		$this->assertTrue(isset($result['create'][1]['accounts'][0]));
		$this->assertTrue(isset($result['create'][1]['accounts'][1]));

		$result = Exporter::get('update', $array->export());
		$this->assertTrue(isset($result['update'][0]));
		$this->assertTrue(isset($result['update'][1]));
		$this->assertFalse(isset($result['update']['4c8f86167675abfabd970300']));
		$this->assertFalse(isset($result['update']['4c8f86167675abfabd970301']));
		$this->assertTrue(isset($result['update'][0]['accounts'][0]));
		$this->assertTrue(isset($result['update'][0]['accounts'][1]));
		$this->assertTrue(isset($result['update'][1]['accounts'][0]));
		$this->assertTrue(isset($result['update'][1]['accounts'][1]));
	}

	public function testIndexesOnExportingDocument() {
		$schema = new Schema(array('fields' => array(
			'_id' => array('type' => 'id'),
			'accounts' => array('type' => 'object', 'array' => true),
			'accounts._id' => array('type' => 'id'),
			'accounts.name' => array('type' => 'string')
		)));

		$data = array(
			'_id' => '4c8f86167675abfabd970300',
			'accounts' => array(array(
				'_id' => "4fb6e2dd3e91581fe6e75736",
				'name' => 'Foo1'
			), array(
				'_id' => "4fb6e2df3e91581fe6e75737",
				'name' => 'Bar1'
			))
		);

		$model = $this->_model;

		$document = new Document(compact('model', 'schema', 'data'));
		$this->assertTrue($document->accounts instanceof DocumentSet);
		$this->assertTrue($document->accounts instanceof DocumentSet);

		$export = $document->export();
		$result = Exporter::get('create', $document->export());
		$this->assertTrue(isset($result['create']['accounts'][0]));
		$this->assertTrue(isset($result['create']['accounts'][1]));

		$export['data'] = array();
		$result = Exporter::get('update', $export);
		$this->assertTrue(isset($result['update']['accounts'][0]));
		$this->assertTrue(isset($result['update']['accounts'][1]));
	}

	public function testEmptyArrayAsDocument() {
		$schema = new Schema(array('fields' => array(
			'_id' => array('type' => 'id'),
			'accounts' => array('type' => 'object', 'array' => true),
			'accounts.name' => array('type' => 'string')
		)));

		$data = array(
			'_id' => '4c8f86167675abfabd970300',
			'accounts' => array(array())
		);

		$model = $this->_model;

		$document = new Document(compact('model', 'schema', 'data'));
		$this->assertTrue($document->accounts[0] instanceof Document);
	}
}

?>