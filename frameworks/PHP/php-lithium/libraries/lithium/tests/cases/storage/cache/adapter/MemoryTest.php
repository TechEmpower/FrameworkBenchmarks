<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\storage\cache\adapter;

use lithium\storage\cache\adapter\Memory;

class MemoryTest extends \lithium\test\Unit {

	public function setUp() {
		$this->Memory = new Memory();
	}

	public function tearDown() {
		unset($this->Memory);
	}

	public function testEnabled() {
		$memory = $this->Memory;
		$this->assertTrue($memory::enabled());
	}

	public function testWriteAndRead() {
		$key = 'key';
		$data = 'data';
		$expiry = null;

		$closure = $this->Memory->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data', 'expiry');
		$result = $closure($this->Memory, $params, null);
		$this->assertTrue($result);
		$this->assertEqual($this->Memory->cache, $result);

		$closure = $this->Memory->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Memory, $params, null);
		$this->assertEqual($data, $result);
		$this->assertEqual($this->Memory->cache, array($key => $data));
	}

	public function testMultiWriteAndRead() {
		$key = array('write1' => 'value1', 'write2' => 'value2');
		$data = null;
		$expiry = null;

		$closure = $this->Memory->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data', 'expiry');
		$result = $closure($this->Memory, $params, null);
		$this->assertTrue($result);
		$this->assertEqual($this->Memory->cache, $result);

		$closure = $this->Memory->read(array_keys($key));
		$this->assertTrue(is_callable($closure));

		$params = array('key' => array_keys($key));
		$result = $closure($this->Memory, $params, null);
		$this->assertEqual($key, $result);
	}

	public function testWriteAndDelete() {
		$key = 'key_to_delete';
		$data = 'some data to be deleted';
		$expiry = null;

		$closure = $this->Memory->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data');
		$result = $closure($this->Memory, $params, null);
		$this->assertTrue($result);
		$this->assertEqual($this->Memory->cache, $result);

		$closure = $this->Memory->delete($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Memory, $params, null);
		$this->assertTrue($result);

		$key = 'non_existent';
		$params = compact('key');
		$result = $closure($this->Memory, $params, null);
		$this->assertFalse($result);
	}

	public function testWriteAndClear() {
		$key = 'key_to_clear';
		$data = 'data to be cleared';
		$expiry = null;

		$closure = $this->Memory->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data');
		$result = $closure($this->Memory, $params, null);
		$this->assertTrue($result);
		$this->assertEqual($this->Memory->cache, $result);

		$key2 = 'key2_to_clear';
		$data2 = 'data to be cleared';

		$closure = $this->Memory->write($key2, $data2, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = array('key' => $key2, 'data' => $data2);
		$result = $closure($this->Memory, $params, null);
		$this->assertTrue($result);
		$this->assertEqual($this->Memory->cache, $result);

		$result = $this->Memory->clear();
		$this->assertTrue($result);
		$this->assertEqual(array(), $this->Memory->cache);

		$closure = $this->Memory->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data');
		$result = $closure($this->Memory, $params, null);
		$this->assertTrue($result);
		$this->assertEqual($this->Memory->cache, $result);

		$result = $this->Memory->clear();
		$this->assertTrue($result);
		$this->assertEqual(array(), $this->Memory->cache);

	}

	public function testIncrement() {
		$key = 'incremental';
		$data = 5;
		$expiry = null;

		$closure = $this->Memory->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data');
		$result = $closure($this->Memory, $params, null);
		$this->assertTrue($result);
		$this->assertEqual($this->Memory->cache, $result);

		$closure = $this->Memory->increment($key);
		$params = compact('key');

		$result = $closure($this->Memory, $params, null);
		$this->assertEqual($data + 1, $result);
	}

	public function testDecrement() {
		$key = 'decrement';
		$data = 5;
		$expiry = null;

		$closure = $this->Memory->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data');
		$result = $closure($this->Memory, $params, null);
		$this->assertTrue($result);
		$this->assertEqual($this->Memory->cache, $result);

		$closure = $this->Memory->decrement($key);
		$params = compact('key');

		$result = $closure($this->Memory, $params, null);
		$this->assertEqual($data - 1, $result);
	}

	public function testClean() {
		$result = $this->Memory->clean();
		$this->assertFalse($result);
	}
}

?>