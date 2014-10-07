<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of Rad, Inc. (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\storage\session\adapter;

use lithium\storage\session\adapter\Memory;

class MemoryTest extends \lithium\test\Unit {

	/**
	 * Initializes a new `Memory` adapter.
	 */
	public function setUp() {
		$this->Memory = new Memory();
	}

	/**
	 * Unset the memory adapter.
	 */
	public function tearDown() {
		unset($this->Memory);
	}

	/**
	 * Tests if a correct (and unique) key is loaded upon request.
	 */
	public function testKey() {
		$key1 = Memory::key();
		$this->assertTrue($key1);

		$key2 = Memory::key();
		$this->assertNotEqual($key1, $key2);

		$pattern = '/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/';
		$this->assertPattern($pattern, Memory::key());
	}

	/**
	 * This adapter is always enabled by default as it does not rely on any external
	 * dependencies.
	 */
	public function testEnabled() {
		$this->assertTrue(Memory::enabled());
	}

	/**
	 * This adapter is always started when a new object is generated.
	 */
	public function testIsStarted() {
		$this->assertTrue($this->Memory->isStarted());
	}

	/**
	 * Test if reading from the memory adapter works as expected.
	 */
	public function testRead() {
		$this->Memory->read();

		$key = 'read_test';
		$value = 'value to be read';

		$this->Memory->_session[$key] = $value;

		$closure = $this->Memory->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Memory, $params, null);

		$this->assertIdentical($value, $result);

		$key = 'non-existent';
		$closure = $this->Memory->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Memory, $params, null);
		$this->assertNull($result);

		$closure = $this->Memory->read();
		$this->assertTrue(is_callable($closure));

		$result = $closure($this->Memory, array('key' => null), null);
		$expected = array('read_test' => 'value to be read');
		$this->assertEqual($expected, $result);
	}

	/**
	 * Writes test data into the $_session array.
	 */
	public function testWrite() {
		$key = 'write-test';
		$value = 'value to be written';

		$closure = $this->Memory->write($key, $value);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'value');
		$result = $closure($this->Memory, $params, null);
		$this->assertEqual($this->Memory->_session[$key], $value);
	}

	/**
	 * Checks if the session data is empty on creation.
	 */
	public function testCheck() {
		$this->Memory->read();

		$key = 'read';
		$value = 'value to be read';
		$this->Memory->_session[$key] = $value;

		$closure = $this->Memory->check($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Memory, $params, null);
		$this->assertTrue($result);

		$key = 'does_not_exist';
		$closure = $this->Memory->check($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Memory, $params, null);
		$this->assertFalse($result);
	}

	/**
	 * Test key deletion.
	 */
	public function testDelete() {
		$this->Memory->read();

		$key = 'delete_test';
		$value = 'value to be deleted';

		$this->Memory->_session[$key] = $value;

		$closure = $this->Memory->delete($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Memory, $params, null);
		$this->assertTrue($result);

		$key = 'non-existent';
		$closure = $this->Memory->delete($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Memory, $params, null);
		$this->assertTrue($result);
	}

	/**
	 * Checks if erasing the whole session array works as expected.
	 */
	public function testClear() {
		$this->Memory->_session['foobar'] = 'foo';
		$closure = $this->Memory->clear();
		$this->assertTrue(is_callable($closure));
		$result = $closure($this->Memory, array(), null);
		$this->assertTrue(empty($this->Memory->_session));
	}
}

?>