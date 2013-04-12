<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\data\collection;

use lithium\data\collection\MultiKeyRecordSet;
use lithium\tests\mocks\data\collection\MockMultiKeyRecordSet;
use lithium\tests\mocks\data\model\mock_database\MockResult;
use lithium\tests\mocks\data\MockPostObject;
use lithium\util\Collection;

/**
 * RecordSet tests
 */
class MultiKeyRecordSetTest extends \lithium\test\Unit {

	protected $_model = 'lithium\tests\mocks\data\MockModel';
	protected $_model2 = 'lithium\tests\mocks\data\MockModelCompositePk';

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

		$this->_recordSet = new MockMultiKeyRecordSet(compact('result', 'model') + array(
			'exists' => true
		));

		$result = new MockResult(array('records' => $this->_records));

		foreach ($this->_records as $i => $record) {
			$this->_objectRecords[$i] = new MockPostObject($record);
		}
		$this->_objectRecordSet = new MockMultiKeyRecordSet(compact('result', 'model') + array(
			'exists' => true
		));
	}

	public function tearDown() {
		Collection::formats(false);
	}

	public function testInit() {
		$recordSet = new MockMultiKeyRecordSet();
		$this->assertTrue($recordSet instanceof MultiKeyRecordSet);

		$recordSet = new MockMultiKeyRecordSet(array(
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

		$data = array(array(
				'client_id' => 1,
				'invoice_id' => 4,
				'title' => 'Payment1'
			), array(
				'client_id' => 2,
				'invoice_id' => 5,
				'title' => 'Payment2'
			), array(
				'client_id' => 3,
				'invoice_id' => 6,
				'title' => 'Payment3'
		));

		$payments = new MockMultiKeyRecordSet(array('data' => $data, 'model' => $this->_model2));
		$this->assertTrue(isset($payments[array('client_id' => 1,'invoice_id' => 4)]));
		$this->assertTrue(isset($payments[array('invoice_id' => 4, 'client_id' => 1)]));
		$this->assertFalse(isset($payments[0]));
		$this->assertFalse(isset($payments[true]));
		$this->assertFalse(isset($payments[false]));
		$this->assertFalse(isset($payments[null]));
		$this->assertFalse(isset($payments['string']));

		$records = new MockMultiKeyRecordSet();
		$records[0] = array('title' => 'Record0');
		$records[1] = array('title' => 'Record1');
		$this->assertTrue(isset($records[true]));
		$this->assertTrue(isset($records[null]));
		$this->assertTrue(isset($records[false]));
		$this->assertTrue(isset($records[array()]));
		$this->assertTrue(isset($records[0]));
		$this->assertTrue(isset($records['0']));
		$this->assertTrue(isset($records[1]));
		$this->assertTrue(isset($records['1']));
		$this->assertFalse(isset($records[2]));
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

		$recordSet = new MockMultiKeyRecordSet();
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

		$recordSet = new MockMultiKeyRecordSet(compact('result', 'model'));

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

		$this->_objectRecordSet[1] = new MockPostObject(array('id' => 1, 'data' => 'new data1'));

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

		$data = array(array(
				'client_id' => 1,
				'invoice_id' => 4,
				'title' => 'Payment1'
			), array(
				'client_id' => 2,
				'invoice_id' => 5,
				'title' => 'Payment2'
			), array(
				'client_id' => 3,
				'invoice_id' => 6,
				'title' => 'Payment3'
		));

		$payments = new MockMultiKeyRecordSet(array('data' => $data, 'model' => $this->_model2));

		$expected = array(array(
				'client_id' => 2,
				'invoice_id' => 5,
				'title' => 'Payment2'
			), array(
				'client_id' => 3,
				'invoice_id' => 6,
				'title' => 'Payment3'
		));

		unset($payments[array('client_id' => 1,'invoice_id' => 4)]);
		$this->assertEqual($expected, array_values($payments->data()));

		$payments = new MockMultiKeyRecordSet(array('data' => $data, 'model' => $this->_model2));
		unset($payments[array('invoice_id' => 4, 'client_id' => 1)]);
		$this->assertEqual($expected, array_values($payments->data()));

		unset($payments[true]);
		$this->assertEqual($expected, array_values($payments->data()));

		unset($payments[false]);
		$this->assertEqual($expected, array_values($payments->data()));

		unset($payments[null]);
		$this->assertEqual($expected, array_values($payments->data()));

		unset($payments['string']);
		$this->assertEqual($expected, array_values($payments->data()));
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
			array('id' => 1, 'data' => 'data1'),
			array('id' => 2, 'data' => 'data2'),
			array('id' => 3, 'data' => 'data3'),
			array('id' => 4, 'data' => 'data4')
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
		$expected = array('1data1', '2data2', '3data3', '4data4');

		$result = $this->_recordSet->map($filter, array('collect' => false));
		$this->assertEqual($expected, $result);

		$result = $this->_recordSet->map($filter);

		$this->assertEqual($expected, $result->get('_data'));

		$result = $this->_objectRecordSet->map($filter, array('collect' => false));
		$this->assertEqual($expected, $result);

		$result = $this->_objectRecordSet->map($filter);
		$this->assertEqual($expected, $result->get('_data'));
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
		$posts = new MockMultiKeyRecordSet(array('data' => $expected));
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

		$posts = new MockMultiKeyRecordSet();
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

		$recordSet = new MockMultiKeyRecordSet(compact('result', 'model'));
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

		$recordSet = new MockMultiKeyRecordSet(compact('result', 'model') + array(
			'exists' => true
		));

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

		$recordSet = new MockMultiKeyRecordSet(compact('result', 'model') + array(
			'exists' => true
		));

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
		$collection = new MultiKeyRecordSet();
		$this->assertFalse($collection->valid());

		$collection = new MultiKeyRecordSet(array('data' => array('value' => 42)));
		$this->assertTrue($collection->valid());

		$resource = new MockResult(array('records' => array()));
		$collection = new MultiKeyRecordSet(array('model' => $this->_model, 'result' => $resource));
		$this->assertFalse($collection->valid());

		$resource = new MockResult(array(
			'records' => array(array('id' => 1, 'data' => 'data1'))
		));
		$collection = new MultiKeyRecordSet(array('model' => $this->_model, 'result' => $resource));
		$this->assertTrue($collection->valid());
	}

	public function testRecordWithCombinedPk() {
		$data = array(array(
				'client_id' => 1,
				'invoice_id' => 4,
				'title' => 'Payment1'
			), array(
				'client_id' => 2,
				'invoice_id' => 5,
				'title' => 'Payment2'
			), array(
				'client_id' => 3,
				'invoice_id' => 6,
				'title' => 'Payment3'
		));

		$payments = new MockMultiKeyRecordSet(array('data' => $data, 'model' => $this->_model2));
		$this->assertEqual(3, count($payments->get('_data')));

		$index = array('client_id' => 1, 'invoice_id' => 4);
		$this->assertEqual($data[0], $payments[$index]->data());

		$index = array('client_id' => 3, 'invoice_id' => 6);
		$this->assertEqual($data[2], $payments[$index]->data());

		$this->assertNull($payments[array('client_id' => 3, 'invoice_id' => 3)]);
		$this->assertNull($payments[array('client_id' => 2)]);
		$this->assertNull($payments[array('invoice_id' => 6)]);

		$index = array('client_id' => 2, 'invoice_id' => 5);
		$this->assertEqual($data[1], $payments[$index]->data());
	}

	public function testKeyCastingManagment() {
		$payments = new MockMultiKeyRecordSet();
		$payments[true] = array('title' => 'Payment1');
		$payments[null] = array('title' => 'Payment2');
		$payments[false] = array('title' => 'Payment3');
		$payments[array()] = array('title' => 'Payment4');

		$expected = array(
			0 => array('title' => 'Payment1'),
			1 => array('title' => 'Payment2'),
			2 => array('title' => 'Payment3'),
			3 => array('title' => 'Payment4')
		);

		$this->assertEqual($expected, $payments->data());

		$expected = array('title' => 'Payment1 updated');
		$payments[0] = $expected;
		$this->assertEqual($expected, $payments[0]);

		$expected = array('title' => 'Payment1 updated 2');
		$payments['0'] = $expected;
		$this->assertEqual($expected, $payments['0']);
		$this->assertEqual($expected, $payments[0]);
	}

	public function testRecordWithCombinedPkAndLazyLoading() {

		$records = array(
			array('client_id' => 1, 'invoice_id' => 4, 'title' => 'Payment1'),
			array('client_id' => 2, 'invoice_id' => 5, 'title' => 'Payment2'),
			array('client_id' => 2, 'invoice_id' => 6, 'title' => 'Payment3'),
			array('client_id' => 4, 'invoice_id' => 7, 'title' => 'Payment3')
		);

		$result = new MockResult(array('records' => $records));

		$payments = new MockMultiKeyRecordSet(array(
			'result' => $result, 'model' => $this->_model2
		));
		$this->assertEqual(0, count($payments->get('_data')));

		$result = $payments[array('client_id' => 1, 'invoice_id' => 4)]->to('array');
		$this->assertEqual($records[0], $result);

		$result = $payments[array('client_id' => 2, 'invoice_id' => 6)]->to('array');
		$this->assertEqual($records[2], $result);
		$this->assertEqual(3, count($payments->get('_data')));

		$result = $payments[array('client_id' => 2, 'invoice_id' => 5)]->to('array');
		$this->assertEqual($records[1], $result);
		$this->assertEqual(3, count($payments->get('_data')));

		$this->assertNull($payments[array('client_id' => 3, 'invoice_id' => 3)]);
		$this->assertNull($payments[array('client_id' => 2)]);
		$this->assertNull($payments[array('invoice_id' => 6)]);

		$this->assertEqual(4, count($payments->get('_data')));

		$this->assertTrue($payments->reset());
		$this->assertEqual(0, count($payments->get('_data')));

		$this->assertEqual($records, $payments->to('array'));

		$expected = '[{"client_id":1,"invoice_id":4,"title":"Payment1"},' .
					'{"client_id":2,"invoice_id":5,"title":"Payment2"},' .
					'{"client_id":2,"invoice_id":6,"title":"Payment3"},' .
					'{"client_id":4,"invoice_id":7,"title":"Payment3"}]';

		Collection::formats('lithium\net\http\Media');
		$this->assertEqual($expected, $payments->to('json'));
	}

	public function testInternalWithCombinedPkKeys() {
		$data = array(array(
				'client_id' => 1,
				'invoice_id' => 4,
				'title' => 'Payment1'
			), array(
				'client_id' => 2,
				'invoice_id' => 5,
				'title' => 'Payment2'
			), array(
				'client_id' => 3,
				'invoice_id' => 6,
				'title' => 'Payment3'
		));

		$payments = new MockMultiKeyRecordSet(array('data' => $data, 'model' => $this->_model2));

		$expected = array( array(
				'client_id' => 1,
				'invoice_id' => 4
			), array(
				'client_id' => 2,
				'invoice_id' => 5
			), array(
				'client_id' => 3,
				'invoice_id' => 6
		));
		$this->assertEqual($expected, $payments->keys());
	}

	public function testInternalKeys() {
		$this->assertEqual(array(0 => 1, 1 => 2, 2 => 3, 3 => 4), $this->_recordSet->keys());
		$this->assertEqual(array(0 => 1, 1 => 2, 2 => 3, 3 => 4), $this->_objectRecordSet->keys());
	}
}

?>