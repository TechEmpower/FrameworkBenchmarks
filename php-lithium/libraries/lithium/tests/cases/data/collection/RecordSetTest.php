<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\data\collection;

use lithium\data\collection\RecordSet;
use lithium\tests\mocks\data\collection\MockRecordSet;
use lithium\tests\mocks\data\model\mock_database\MockResult;
use lithium\tests\mocks\data\MockPostObject;
use lithium\util\Collection;

/**
 * RecordSet tests
 */
class RecordSetTest extends \lithium\test\Unit {

	protected $_model = 'lithium\tests\mocks\data\MockModel';

	/**
	 * RecordSet object to test
	 *
	 * @var object
	 */
	protected $_recordSet = null;

	/**
	 * Object based RecordSet object to test
	 *
	 * @var object
	 */
	protected $_objectRecordSet = null;

	/**
	 * Array of records for testing
	 *
	 * @var array
	 */
	protected $_records = array(
		array('id' => 1, 'data' => 'data1'),
		array('id' => 2, 'data' => 'data2'),
		array('id' => 3, 'data' => 'data3'),
		array('id' => 4, 'data' => 'data4')
	);

	/**
	 * Array of object records for testing
	 *
	 * @var array
	 */
	protected $_objectRecords = array();

	public function setUp() {
		$result = new MockResult(array('records' => $this->_records));

		$model = $this->_model;

		$this->_recordSet = new MockRecordSet(compact('result', 'model') + array(
			'exists' => true
		));

		$result = new MockResult(array('records' => $this->_records));

		foreach ($this->_records as $i => $record) {
			$this->_objectRecords[$i] = new MockPostObject($record);
		}
		$this->_objectRecordSet = new MockRecordSet(compact('result', 'model') + array(
			'exists' => true
		));
	}

	public function tearDown() {
		Collection::formats(false);
	}

	public function testInit() {
		$recordSet = new MockRecordSet();
		$this->assertTrue($recordSet instanceof RecordSet);

		$recordSet = new MockRecordSet(array(
			'model'  => $this->_model,
			'result' => true,
			'exists' => true
		));

		$this->assertEqual($this->_model, $recordSet->model());
		$this->assertTrue($recordSet->get('_result'));
	}

	public function testOffsetExists() {
		$this->assertFalse($this->_recordSet->offsetExists(0));
		$this->assertTrue($this->_recordSet->offsetExists(1));
		$this->assertTrue($this->_recordSet->offsetExists(2));
		$this->assertTrue($this->_recordSet->offsetExists(3));
		$this->assertTrue($this->_recordSet->offsetExists(4));

		$this->assertTrue(isset($this->_recordSet[3]));

		$this->assertFalse($this->_objectRecordSet->offsetExists(0));
		$this->assertTrue($this->_objectRecordSet->offsetExists(1));
		$this->assertTrue($this->_objectRecordSet->offsetExists(2));
		$this->assertTrue($this->_objectRecordSet->offsetExists(3));
		$this->assertTrue($this->_objectRecordSet->offsetExists(4));
		$this->assertTrue(isset($this->_objectRecordSet[3]));
	}

	public function testOffsetGet() {
		$expected = array('id' => 1, 'data' => 'data1');
		$this->assertEqual($expected, $this->_recordSet[1]->to('array'));

		$expected = array('id' => 2, 'data' => 'data2');
		$this->assertEqual($expected, $this->_recordSet[2]->to('array'));

		$expected = array('id' => 3, 'data' => 'data3');
		$this->assertEqual($expected, $this->_recordSet[3]->to('array'));

		$expected = array('id' => 4, 'data' => 'data4');
		$this->assertEqual($expected, $this->_recordSet[4]->to('array'));

		$expected = array('id' => 3, 'data' => 'data3');
		$this->assertEqual($this->_records[2], $this->_recordSet[3]->to('array'));

		$recordSet = new MockRecordSet();
		$this->assertEqual(array(), $recordSet->data());

		$this->assertNull($this->_recordSet[5]);
	}

	public function testWithNoIndexes() {
		$records = array(
			array('data' => 'data1'),
			array('data' => 'data2'),
			array('data' => 'data3'),
			array('data' => 'data4')
		);

		$result = new MockResult(array('records' => $records));

		$model = $this->_model;

		$recordSet = new MockRecordSet(compact('result', 'model'));

		$this->assertEqual($records, $recordSet->data());
		$this->assertEqual($records[1]['data'], $recordSet[1]->data);
	}

	public function testOffsetGetObject() {
		$result = $this->_objectRecordSet[1];
		$this->assertEqual(1, $result->id);
		$this->assertEqual('data1', $result->data);

		$result = $this->_objectRecordSet[2];
		$this->assertEqual(2, $result->id);
		$this->assertEqual('data2', $result->data);

		$result = $this->_objectRecordSet[3];
		$this->assertEqual(3, $result->id);
		$this->assertEqual('data3', $result->data);

		$result = $this->_objectRecordSet[4];
		$this->assertEqual(4, $result->id);
		$this->assertEqual('data4', $result->data);

		$this->assertNull($this->_objectRecordSet[5]);
	}

	public function testOffsetGetBackwards() {
		$expected = array('id' => 4, 'data' => 'data4');
		$this->assertEqual($expected, $this->_recordSet[4]->to('array'));

		$expected = array('id' => 3, 'data' => 'data3');
		$this->assertEqual($expected, $this->_recordSet[3]->to('array'));

		$expected = array('id' => 2, 'data' => 'data2');
		$this->assertEqual($expected, $this->_recordSet[2]->to('array'));

		$expected = array('id' => 1, 'data' => 'data1');
		$this->assertEqual($expected, $this->_recordSet[1]->to('array'));

		$result = $this->_objectRecordSet[4];
		$this->assertEqual(4, $result->id);
		$this->assertEqual('data4', $result->data);

		$result = $this->_objectRecordSet[3];
		$this->assertEqual(3, $result->id);
		$this->assertEqual('data3', $result->data);

		$result = $this->_objectRecordSet[2];
		$this->assertEqual(2, $result->id);
		$this->assertEqual('data2', $result->data);

		$result = $this->_objectRecordSet[1];
		$this->assertEqual(1, $result->id);
		$this->assertEqual('data1', $result->data);
	}

	public function testOffsetSet() {
		$this->assertEqual(0, count($this->_recordSet->get('_data')));
		$this->_recordSet[5] = $expected = array('id' => 5, 'data' => 'data5');
		$this->assertEqual($expected, $this->_recordSet[5]->to('array'));
		$this->assertEqual(5, count($this->_recordSet->get('_data')));

		$this->_recordSet[] = $expected = array('id' => 6, 'data' => 'data6');
		$this->assertEqual($expected, $this->_recordSet[6]->to('array'));
		$this->assertEqual(6, count($this->_recordSet->get('_data')));

		$this->_objectRecordSet[5] = $expected = new MockPostObject(array(
			'id' => 5, 'data' => 'data5'
		));
		$item = $this->_objectRecordSet[5];
		$this->assertEqual($expected->id, $item->id);
		$this->assertEqual($expected->data, $item->data);

		$this->_objectRecordSet[] = $expected = new MockPostObject(array(
			'id' => 6, 'data' => 'data6 new'
		));
		$item = $this->_objectRecordSet[6];
		$this->assertEqual($expected->id, $item->id);
		$this->assertEqual($expected->data, $item->data);

		$this->_objectRecordSet[] = $expected = new MockPostObject(array(
			'id' => 6, 'data' => 'data6 new2'
		));
		$item = $this->_objectRecordSet[6];
		$this->assertEqual($expected->id, $item->id);
		$this->assertEqual($expected->data, $item->data);
	}

	public function testOffsetSetWithLoadedData() {
		$this->_recordSet[1] = array('id' => 1, 'data' => 'new data1');

		$expected = array(
			1 => array('id' => 1, 'data' => 'new data1'),
			2 => array('id' => 2, 'data' => 'data2'),
			3 => array('id' => 3, 'data' => 'data3'),
			4 => array('id' => 4, 'data' => 'data4')
		);
		$this->assertEqual($expected, $this->_recordSet->to('array'));

		$this->_objectRecordSet[1] = new MockPostObject(array(
			'id' => 1, 'data' => 'new data1'
		));

		$result = $this->_objectRecordSet[1];
		$this->assertEqual(1, $result->id);
		$this->assertEqual('new data1', $result->data);

		$result = $this->_objectRecordSet[2];
		$this->assertEqual(2, $result->id);
		$this->assertEqual('data2', $result->data);

		$result = $this->_objectRecordSet[3];
		$this->assertEqual(3, $result->id);
		$this->assertEqual('data3', $result->data);

		$result = $this->_objectRecordSet[4];
		$this->assertEqual(4, $result->id);
		$this->assertEqual('data4', $result->data);
	}

	public function testOffsetUnset() {
		unset($this->_recordSet[1]);

		$expected = array(
			2 => array('id' => 2, 'data' => 'data2'),
			3 => array('id' => 3, 'data' => 'data3'),
			4 => array('id' => 4, 'data' => 'data4')
		);
		$this->assertEqual($expected, $this->_recordSet->to('array'));

		unset($this->_objectRecordSet[1]);

		$this->assertNull($this->_objectRecordSet[1]);

		$result = $this->_objectRecordSet[2];
		$this->assertEqual(2, $result->id);
		$this->assertEqual('data2', $result->data);

		$result = $this->_objectRecordSet[3];
		$this->assertEqual(3, $result->id);
		$this->assertEqual('data3', $result->data);

		$result = $this->_objectRecordSet[4];
		$this->assertEqual(4, $result->id);
		$this->assertEqual('data4', $result->data);
	}

	public function testRewind() {
		$this->_recordSet->rewind();

		$expected = array('id' => 1, 'data' => 'data1');
		$this->assertEqual($expected, $this->_recordSet->current()->to('array'));

		$this->_objectRecordSet->rewind();

		$result = $this->_objectRecordSet->current();
		$this->assertEqual(1, $result->id);
		$this->assertEqual('data1', $result->data);
	}

	public function testCurrent() {
		$this->assertEqual($this->_records[0], $this->_recordSet->current()->to('array'));
		$this->assertEqual($this->_records[1], $this->_recordSet->next()->to('array'));
		$this->assertEqual($this->_records[1], $this->_recordSet->current()->to('array'));


		$this->assertEqual($this->_records[0], $this->_recordSet->rewind()->to('array'));
		$this->assertEqual($this->_records[1], $this->_recordSet->next()->to('array'));
		$this->assertEqual($this->_records[1], $this->_recordSet->current()->to('array'));

		$this->assertEqual($this->_records[0], $this->_recordSet->rewind()->to('array'));
		$this->assertEqual($this->_records[0], $this->_recordSet->current()->to('array'));
		$this->assertEqual($this->_records[1], $this->_recordSet->next()->to('array'));

		$this->assertEqual($this->_records[0], $this->_recordSet->rewind()->to('array'));
		$this->assertEqual($this->_records[1], $this->_recordSet->next()->to('array'));
		$this->assertEqual($this->_records[2], $this->_recordSet->next()->to('array'));

		$result = $this->_objectRecordSet->current();
		$this->assertEqual($this->_objectRecordSet[1]->id, $result->id);
		$this->assertEqual($this->_objectRecordSet[1]->data, $result->data);
		$this->_objectRecordSet->next();
		$result = $this->_objectRecordSet->current();
		$this->assertEqual($this->_objectRecordSet[2]->id, $result->id);
		$this->assertEqual($this->_objectRecordSet[2]->data, $result->data);


		$result = $this->_objectRecordSet->rewind();
		$this->assertEqual($this->_objectRecordSet[1]->id, $result->id);
		$this->assertEqual($this->_objectRecordSet[1]->data, $result->data);
		$result = $this->_objectRecordSet->next();
		$this->assertEqual($this->_objectRecordSet[2]->id, $result->id);
		$this->assertEqual($this->_objectRecordSet[2]->data, $result->data);

		$this->_objectRecordSet->rewind();
		$result = $this->_objectRecordSet->current();
		$this->assertEqual($this->_objectRecordSet[1]->id, $result->id);
		$this->assertEqual($this->_objectRecordSet[1]->data, $result->data);
		$result = $this->_objectRecordSet->next();
		$this->assertEqual($this->_objectRecordSet[2]->id, $result->id);
		$this->assertEqual($this->_objectRecordSet[2]->data, $result->data);

		$this->_objectRecordSet->rewind();
		$result = $this->_objectRecordSet->next();
		$this->assertEqual($this->_objectRecordSet[2]->id, $result->id);
		$this->assertEqual($this->_objectRecordSet[2]->data, $result->data);
	}

	public function testKey() {
		$this->_recordSet->current();
		$this->assertEqual(1, $this->_recordSet->key());

		$this->_recordSet->next();
		$this->assertEqual(2, $this->_recordSet->key());
	}

	public function testNextWithForEach() {
		$counter = 0;
		foreach ($this->_recordSet as $record) {
			$this->assertEqual($this->_records[$counter], $record->to('array'));
			$counter++;
		}
		$this->assertEqual(4, $counter);

		$counter = 0;
		foreach ($this->_objectRecordSet as $record) {
			$this->assertEqual($this->_objectRecords[$counter]->id, $record->id);
			$this->assertEqual($this->_objectRecords[$counter]->data, $record->data);
			$counter++;
		}
		$this->assertEqual(4, $counter);
	}

	public function testNextWithWhile() {
		$counter = 0;
		while ($this->_recordSet->key() !== null) {
			$record = $this->_recordSet->current();
			$this->assertEqual($this->_records[$counter], $record->to('array'));
			$counter++;
			$this->_recordSet->next();
		}
		$this->assertEqual(4, $counter);

		$counter = 0;
		while ($this->_objectRecordSet->key() !== null) {
			$record = $this->_objectRecordSet->current();
			$this->assertEqual($this->_objectRecords[$counter]->id, $record->id);
			$this->assertEqual($this->_objectRecords[$counter]->data, $record->data);
			$counter++;
			$this->_objectRecordSet->next();
		}
		$this->assertEqual(4, $counter);
	}

	public function testMeta() {
		$expected = array('model' => 'lithium\tests\mocks\data\MockModel');
		$this->assertEqual($expected, $this->_recordSet->meta());

		$expected = array('model' => 'lithium\tests\mocks\data\MockModel');
		$this->assertEqual($expected, $this->_objectRecordSet->meta());
	}

	public function testTo() {
		Collection::formats('lithium\net\http\Media');
		$this->assertFalse(isset($this->_recordSet[0]));
		$expected = array(
			1 => array('id' => 1, 'data' => 'data1'),
			2 => array('id' => 2, 'data' => 'data2'),
			3 => array('id' => 3, 'data' => 'data3'),
			4 => array('id' => 4, 'data' => 'data4')
		);
		$this->assertEqual($expected, $this->_recordSet->to('array'));

		$expected = '{"1":{"id":1,"data":"data1"},"2":{"id":2,"data":"data2"},';
		$expected .= '"3":{"id":3,"data":"data3"},"4":{"id":4,"data":"data4"}}';
		$this->assertEqual($expected, $this->_recordSet->to('json'));
	}

	public function testToInternal() {
		Collection::formats('lithium\net\http\Media');

		$expected = array(
			array('id' => 1, 'data' => 'data1'),
			array('id' => 2, 'data' => 'data2'),
			array('id' => 3, 'data' => 'data3'),
			array('id' => 4, 'data' => 'data4')
		);
		$this->assertEqual($expected, $this->_recordSet->to('array', array('indexed' => false)));

		$expected = '{"1":{"id":1,"data":"data1"},"2":{"id":2,"data":"data2"},';
		$expected .= '"3":{"id":3,"data":"data3"},"4":{"id":4,"data":"data4"}}';
		$this->assertEqual($expected, $this->_recordSet->to('json'));

		$expected = '[{"id":1,"data":"data1"},{"id":2,"data":"data2"},';
		$expected .= '{"id":3,"data":"data3"},{"id":4,"data":"data4"}]';
		$result = $this->_recordSet->to('json', array('indexed' => false));
		$this->assertEqual($expected, $result);
	}

	public function testRecordSetFindFilter() {
		$expected = array(
			1 => array('id' => 1, 'data' => 'data1'),
			2 => array('id' => 2, 'data' => 'data2'),
			3 => array('id' => 3, 'data' => 'data3'),
			4 => array('id' => 4, 'data' => 'data4')
		);

		$records = $this->_recordSet->find(function($item) {
			return true;
		});
		$this->assertEqual($expected, $records->to('array'));
	}

	public function testEach() {
		$filter = function($rec) {
			$rec->more_data = "More Data{$rec->id}";
			return $rec;
		};
		$expected = array(
			1 => array('id' => 1, 'data' => 'data1', 'more_data' => 'More Data1'),
			2 => array('id' => 2, 'data' => 'data2', 'more_data' => 'More Data2'),
			3 => array('id' => 3, 'data' => 'data3', 'more_data' => 'More Data3'),
			4 => array('id' => 4, 'data' => 'data4', 'more_data' => 'More Data4')
		);
		$result = $this->_recordSet->each($filter)->to('array');
		$this->assertEqual($expected, $result);

		$result = $this->_objectRecordSet->each($filter);
		foreach ($result as $key => $record) {
			$this->assertEqual($expected[$key]['id'], $record->id);
			$this->assertEqual($expected[$key]['data'], $record->data);
			$this->assertEqual($expected[$key]['more_data'], $record->more_data);
		}
	}

	public function testMap() {
		$filter = function($rec) {
			return $rec->id . $rec->data;
		};
		$expected = array(1 => '1data1', 2 => '2data2', 3 => '3data3', 4 => '4data4');

		$result = $this->_recordSet->map($filter, array('collect' => false));
		$this->assertEqual($expected, $result);

		$result = $this->_recordSet->map($filter);

		$this->assertEqual($expected, $result->get('_data'));

		$result = $this->_objectRecordSet->map($filter, array('collect' => false));
		$this->assertEqual($expected, $result);

		$result = $this->_objectRecordSet->map($filter);
		$this->assertEqual($expected, $result->get('_data'));
	}

	public function testReduce() {
		$filter = function($memo, $rec) {
			return $memo + $rec->id;
		};
		$expected = 10;
		$result = $this->_recordSet->reduce($filter, 0);
		$this->assertEqual($expected, $result);
	}

	public function testRecordSet() {
		$expected = array(
			'post1' => array(
				'title' => 'My First Post',
				'content' => 'First Content...'
			),
			'post2' => array(
				'title' => 'My Second Post',
				'content' => 'Also some foobar text'
			),
			'post3' => array(
				'title' => 'My Third Post',
				'content' => 'I like to write some foobar foo too'
			)
		);
		$posts = new MockRecordSet(array('data' => $expected));
		$this->assertEqual(3, count($posts->get('_data')));

		$this->assertEqual($expected['post1'], $posts->first());
		$this->assertEqual($expected['post1'], $posts->current());
		$this->assertEqual($expected['post2'], $posts->next());
		$this->assertEqual($expected['post2'], $posts->current());
		$this->assertEqual($expected['post1'], $posts->prev());
		$this->assertEqual($expected['post2'], $posts->next());
		$this->assertEqual($expected['post3'], $posts->next());
		$this->assertEqual($expected['post3'], $posts->current());
		$this->assertEqual($expected['post2'], $posts->prev());
		$this->assertEqual($expected['post1'], $posts->rewind());
		$this->assertEqual($expected['post1'], $posts->current());
		$this->assertEqual($expected['post1'], $posts['post1']);

		$posts = new MockRecordSet();
		$posts->set($expected);
		$this->assertEqual(3, count($posts->get('_data')));

		$this->assertEqual($expected['post1'], $posts->first());
		$this->assertEqual($expected['post1'], $posts->current());
		$this->assertEqual($expected['post2'], $posts->next());
		$this->assertEqual($expected['post2'], $posts->current());
		$this->assertEqual($expected['post1'], $posts->prev());
		$this->assertEqual($expected['post2'], $posts->next());
		$this->assertEqual($expected['post3'], $posts->next());
		$this->assertEqual($expected['post3'], $posts->current());
		$this->assertEqual($expected['post2'], $posts->prev());
		$this->assertEqual($expected['post1'], $posts->rewind());
		$this->assertEqual($expected['post1'], $posts->current());
		$this->assertEqual($expected['post1'], $posts['post1']);
	}

	public function testRewindReinitialization() {
		$counter = 0;
		while ($record = $this->_recordSet->current()) {
			$this->assertEqual($this->_records[$counter], $record->to('array'));
			$counter++;
			$this->_recordSet->next();
		}
		$this->assertEqual(4, $counter);
		$this->_recordSet->rewind();
		$counter = 0;
		while ($this->_recordSet->key() !== null) {
			$record = $this->_recordSet->current();
			$this->assertEqual($this->_records[$counter], $record->to('array'));
			$this->_recordSet->next();
			$counter++;
		}
		$this->assertEqual(4, $counter);
	}

	public function testRewindResourceOnConstruct() {
		$result = new MockResult(array('records' => $this->_records));

		$model = $this->_model;

		$cpt = 0;
		while ($result->valid()) {
			$result->current();
			$result->next();
			$cpt++;
		}
		$this->assertEqual(4, $cpt);

		$cpt = 0;
		foreach ($result as $value) {
			$cpt++;
		}
		$this->assertEqual(4, $cpt);
		$result->rewind();

		$recordSet = new MockRecordSet(compact('result', 'model'));
		$expected = array(
			1 => array('id' => 1, 'data' => 'data1'),
			2 => array('id' => 2, 'data' => 'data2'),
			3 => array('id' => 3, 'data' => 'data3'),
			4 => array('id' => 4, 'data' => 'data4')
		);
		$result = $recordSet->to('array');
		$this->assertEqual($expected, $result);
	}

	public function testMockResultContent() {
		$result = new MockResult(array('records' => array()));

		$result->rewind();
		$i = 0;
		foreach ($result as $r) {
			$i++;
		}
		$this->assertEqual(0, $i);

		$records = array(
			array('id' => 1, 'data' => 'data1'),
			array('id' => 2, 'data' => 'data2'),
			array('id' => 3, 'data' => 'data3'),
			array('id' => 4, 'data' => 'data4')
		);
		$result = new MockResult(array('records' => $records));

		$i = 0;
		foreach ($result as $s) {
			$this->assertEqual($records[$i], $s);
			$i++;
		}
		$this->assertEqual(4, $i);


		$records = array(
			array(false),
			array('id' => 1, 'data' => 'data1'),
			array('id' => 2, 'data' => 'data2'),
			array('id' => 3, 'data' => 'data3'),
			array('id' => 4, 'data' => 'data4')
		);
		$result = new MockResult(array('records' => $records));

		$i = 0;
		foreach ($result as $s) {
			$this->assertEqual($records[$i], $s);
			$i++;
		}
		$this->assertEqual(5, $i);
	}

	public function testUnsetInForeach() {
		$records = array(
			array('id' => 1, 'data' => 'delete')
		);
		$result = new MockResult(array('records' => $records));

		$model = $this->_model;

		$recordSet = new MockRecordSet(compact('result', 'model') + array('exists' => true));

		$cpt = 0;
		foreach ($recordSet as $i => $word) {
			$array = $word->to('array');
			if ($array['data'] === 'delete') {
				unset($recordSet[$i]);
			}
			$cpt++;
		}

		$this->assertEqual(1, $cpt);
		$this->assertIdentical(array(), $recordSet->to('array'));

		$records = array(
			1 => array('id' => 1, 'data' => 'delete'),
			3 => array('id' => 2, 'data' => 'data2'),
			'hello' => array('id' => 3, 'data' => 'delete'),
			0 => array('id' => 4, 'data' => 'data4'),
			7 => array('id' => 5, 'data' => 'delete'),
			8 => array('id' => 6, 'data' => 'delete'),
			10 => array('id' => 7, 'data' => 'data7'),
			50 => array('id' => 8, 'data' => 'delete')
		);
		$result = new MockResult(array('records' => $records));

		$model = $this->_model;

		$recordSet = new MockRecordSet(compact('result', 'model') + array('exists' => true));

		foreach ($recordSet as $i => $word) {
			$array = $word->to('array');
			if ($array['data'] === 'delete') {
				unset($recordSet[$i]);
			}
		}

		$this->assertEqual(3, count($recordSet));

		$expected = array(
			2 => array('id' => 2, 'data' => 'data2'),
			4 => array('id' => 4, 'data' => 'data4'),
			7 => array('id' => 7, 'data' => 'data7')
		);

		$this->assertIdentical($expected, $recordSet->to('array'));
	}

	public function testValid() {
		$collection = new RecordSet();
		$this->assertFalse($collection->valid());

		$collection = new RecordSet(array('data' => array('value' => 42)));
		$this->assertTrue($collection->valid());

		$resource = new MockResult(array('records' => array()));
		$collection = new RecordSet(array('model' => $this->_model, 'result' => $resource));
		$this->assertFalse($collection->valid());

		$resource = new MockResult(array(
			'records' => array(array('id' => 1, 'data' => 'data1'))
		));
		$collection = new RecordSet(array('model' => $this->_model, 'result' => $resource));
		$this->assertTrue($collection->valid());
	}

	public function testInternalKeys() {
		$this->assertEqual(array(0 => 1, 1 => 2, 2 => 3, 3 => 4), $this->_recordSet->keys());
		$this->assertEqual(array(0 => 1, 1 => 2, 2 => 3, 3 => 4), $this->_objectRecordSet->keys());
	}
}

?>