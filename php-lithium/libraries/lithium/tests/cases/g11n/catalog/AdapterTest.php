<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\g11n\catalog;

use lithium\tests\mocks\g11n\catalog\MockAdapter;

class AdapterTest extends \lithium\test\Unit {

	public $adapter;

	public function setUp() {
		$this->adapter = new MockAdapter();
	}

	public function testReadStubbed() {
		$result = $this->adapter->read(null, null, null);
		$this->assertNull($result);
	}

	public function testWriteStubbed() {
		$result = $this->adapter->write(null, null, null, array());
		$this->assertFalse($result);
	}

	public function testMergeSkipNoId() {
		$item = array(
			'ids' => array('singular' => 'test')
		);
		$expected = array();
		$result = $this->adapter->merge(array(), $item);
		$this->assertEqual($expected, $result);

		$item = array(
			'translated' => array('test')
		);
		$expected = array();
		$result = $this->adapter->merge(array(), $item);
		$this->assertEqual($expected, $result);

		$item = array(
			'id' => null
		);
		$expected = array();
		$result = $this->adapter->merge(array(), $item);
		$this->assertEqual($expected, $result);
	}

	public function testMergeAcceptsSpecialIds() {
		$item = array(
			'id' => '0'
		);
		$result = $this->adapter->merge(array(), $item);
		$this->assertTrue(isset($result['0']));

		$item = array(
			'id' => false
		);
		$result = $this->adapter->merge(array(), $item);
		$this->assertTrue(isset($result[false]));

		$item = array(
			'id' => 0
		);
		$result = $this->adapter->merge(array(), $item);
		$this->assertTrue(isset($result[0]));

		$item = array(
			'id' => 536
		);
		$result = $this->adapter->merge(array(), $item);
		$this->assertTrue(isset($result[536]));

		$item = array(
			'id' => ''
		);
		$result = $this->adapter->merge(array(), $item);
		$this->assertTrue(isset($result['']));
	}

	public function testMergeTranslatedFillIn() {
		$item = array(
			'id' => 'test'
		);
		$data = $this->adapter->merge(array(), $item);

		$item = array(
			'id' => 'test',
			'translated' => array('a')
		);
		$expected = array(
			'test' => array(
				'id' => 'test',
				'ids' => array(),
				'translated' => array('a'),
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
		));
		$result = $this->adapter->merge($data, $item);
		$this->assertEqual($expected, $result);

		$item = array(
			'id' => 'test'
		);
		$data = $this->adapter->merge(array(), $item);

		$item = array(
			'id' => 'test',
			'translated' => 'a'
		);
		$expected = array(
			'test' => array(
				'id' => 'test',
				'ids' => array(),
				'translated' => 'a',
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
		));
		$result = $this->adapter->merge($data, $item);
		$this->assertEqual($expected, $result);
	}

	public function testMergeTranslatedDoNotOverwriteNonArrays() {
		$item = array(
			'id' => 'test',
			'translated' => 'a'
		);
		$data = $this->adapter->merge(array(), $item);

		$item = array(
			'id' => 'test',
			'translated' => 'b'
		);
		$expected = array(
			'test' => array(
				'id' => 'test',
				'ids' => array(),
				'translated' => 'a',
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
		));
		$result = $this->adapter->merge($data, $item);
		$this->assertEqual($expected, $result);
	}

	public function testMergeTranslatedUnionArrays() {
		$item = array(
			'id' => 'test',
			'translated' => array('a')
		);
		$data = $this->adapter->merge(array(), $item);

		$item = array(
			'id' => 'test',
			'translated' => array('b')
		);
		$expected = array(
			'test' => array(
				'id' => 'test',
				'ids' => array(),
				'translated' => array('a'),
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
		));
		$result = $this->adapter->merge($data, $item);
		$this->assertEqual($expected, $result);

		$item = array(
			'id' => 'test',
			'translated' => array('a')
		);
		$data = $this->adapter->merge(array(), $item);

		$item = array(
			'id' => 'test',
			'translated' => array('b', 'c')
		);
		$expected = array(
			'test' => array(
				'id' => 'test',
				'ids' => array(),
				'translated' => array('a', 'c'),
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
		));
		$result = $this->adapter->merge($data, $item);
		$this->assertEqual($expected, $result);
	}

	public function testMergeTranslatedCastToArray() {
		$item = array(
			'id' => 'test',
			'translated' => 'a'
		);
		$data = $this->adapter->merge(array(), $item);

		$item = array(
			'id' => 'test',
			'translated' => array('b', 'c')
		);
		$expected = array(
			'test' => array(
				'id' => 'test',
				'ids' => array(),
				'translated' => array('a', 'c'),
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
		));
		$result = $this->adapter->merge($data, $item);
		$this->assertEqual($expected, $result);
	}

	public function testMergeEnsureDefaultFormat() {
		$item = array(
			'id' => 'test'
		);
		$expected = array(
			'test' => array(
				'id' => 'test',
				'ids' => array(),
				'translated' => null,
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
		));
		$result = $this->adapter->merge(array(), $item);
		$this->assertEqual($expected, $result);
	}

	public function testMergeIds() {
		$item = array(
			'id' => 'test',
			'ids' => array('singular' => 'a')
		);
		$data = $this->adapter->merge(array(), $item);

		$item = array(
			'id' => 'test',
			'ids' => array('singular' => 'X', 'plural' => 'b')
		);
		$expected = array(
			'test' => array(
				'id' => 'test',
				'ids' => array('singular' => 'X', 'plural' => 'b'),
				'translated' => null,
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
		));
		$result = $this->adapter->merge($data, $item);
		$this->assertEqual($expected, $result);
	}

	public function testMergeFlags() {
		$item = array(
			'id' => 'test',
			'flags' => array('fuzzy' => true)
		);
		$data = $this->adapter->merge(array(), $item);

		$item = array(
			'id' => 'test',
			'flags' => array('fuzzy' => false)
		);
		$expected = array(
			'test' => array(
				'id' => 'test',
				'ids' => array(),
				'translated' => null,
				'flags' => array('fuzzy' => false),
				'comments' => array(),
				'occurrences' => array()
		));
		$result = $this->adapter->merge($data, $item);
		$this->assertEqual($expected, $result);

		$item = array(
			'id' => 'test',
			'flags' => array('fuzzy' => false)
		);
		$data = $this->adapter->merge(array(), $item);

		$item = array(
			'id' => 'test',
			'flags' => array('fuzzy' => true)
		);
		$expected = array(
			'test' => array(
				'id' => 'test',
				'ids' => array(),
				'translated' => null,
				'flags' => array('fuzzy' => true),
				'comments' => array(),
				'occurrences' => array()
		));
		$result = $this->adapter->merge($data, $item);
		$this->assertEqual($expected, $result);
	}

	public function testMergeComments() {
		$item = array(
			'id' => 'test',
			'comments' => array('a')
		);
		$data = $this->adapter->merge(array(), $item);

		$item = array(
			'id' => 'test',
			'comments' => array('b')
		);
		$expected = array(
			'test' => array(
				'id' => 'test',
				'ids' => array(),
				'translated' => null,
				'flags' => array(),
				'comments' => array('a', 'b'),
				'occurrences' => array()
		));
		$result = $this->adapter->merge($data, $item);
		$this->assertEqual($expected, $result);
	}

	public function testMergeOccurrences() {
		$item = array(
			'id' => 'test',
			'occurrences' => array(array('file' => 'a.php', 'line' => 2))
		);
		$data = $this->adapter->merge(array(), $item);

		$item = array(
			'id' => 'test',
			'occurrences' => array(array('file' => 'b.php', 'line' => 55))
		);
		$expected = array(
			'test' => array(
				'id' => 'test',
				'ids' => array(),
				'translated' => null,
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array(
					array('file' => 'a.php', 'line' => 2),
					array('file' => 'b.php', 'line' => 55)
				)
		));
		$result = $this->adapter->merge($data, $item);
		$this->assertEqual($expected, $result);
	}
}

?>