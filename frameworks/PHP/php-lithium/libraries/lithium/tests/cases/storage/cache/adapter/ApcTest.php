<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\storage\cache\adapter;

use lithium\storage\cache\adapter\Apc;

class ApcTest extends \lithium\test\Unit {

	/**
	 * Skip the test if APC extension is unavailable.
	 *
	 * @return void
	 */
	public function skip() {
		$this->skipIf(!Apc::enabled(), 'APC is either not loaded or not enabled.');
	}

	public function setUp() {
		apc_clear_cache('user');
		$this->Apc = new Apc();
	}

	public function tearDown() {
		apc_clear_cache('user');
		unset($this->Apc);
	}

	public function testEnabled() {
		$apc = $this->Apc;
		$this->assertTrue($apc::enabled());
	}

	public function testSimpleWrite() {
		$key = 'key';
		$data = 'value';
		$expiry = '+5 seconds';

		$closure = $this->Apc->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data', 'expiry');
		$result = $closure($this->Apc, $params, null);
		$expected = $data;
		$this->assertTrue($result);

		$result = apc_fetch($key);
		$this->assertEqual($expected, $result);

		$result = apc_delete($key);
		$this->assertTrue($result);

		$key = 'another_key';
		$data = 'more_data';
		$expiry = '+1 minute';

		$closure = $this->Apc->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data', 'expiry');
		$result = $closure($this->Apc, $params, null);
		$expected = $data;
		$this->assertTrue($result);

		$result = apc_fetch($key);
		$this->assertEqual($expected, $result);

		$result = apc_delete($key);
		$this->assertTrue($result);
	}

	public function testWriteDefaultCacheTime() {
		$apc = new Apc(array('expiry' => '+5 seconds'));
		$key = 'key';
		$data = 'value';

		$closure = $apc->write($key, $data);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data');
		$result = $closure($apc, $params, null);
		$expected = $data;
		$this->assertTrue($result);

		$result = apc_fetch($key);
		$this->assertEqual($expected, $result);

		$result = apc_delete($key);
		$this->assertTrue($result);
	}

	public function testWriteMulti() {
		$expiry = '+1 minute';
		$key = array(
			'key1' => 'data1',
			'key2' => 'data2',
			'key3' => 'data3'
		);
		$data = null;

		$closure = $this->Apc->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data', 'expiry');
		$result = $closure($this->Apc, $params, null);

		$this->assertEqual(array(), $result);

		$result = apc_fetch(array_keys($key));
		$this->assertEqual($key, $result);

		$result = apc_delete(array_keys($key));
		$this->assertEqual(array(), $result);
	}

	public function testSimpleRead() {
		$key = 'read_key';
		$data = 'read data';

		$result = apc_store($key, $data, 60);
		$this->assertTrue($result);

		$closure = $this->Apc->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Apc, $params, null);
		$expected = $data;
		$this->assertEqual($expected, $result);

		$result = apc_delete($key);
		$this->assertTrue($result);

		$key = 'another_read_key';
		$data = 'read data';

		$result = apc_store($key, $data, 60);
		$this->assertTrue($result);

		$closure = $this->Apc->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Apc, $params, null);
		$expected = $data;

		$this->assertEqual($expected, $result);

		$result = apc_delete($key);
		$this->assertTrue($result);
	}

	public function testReadMulti() {
		$expiry = '+1 minute';
		$key = array(
			'key1' => 'data1',
			'key2' => 'data2',
			'key3' => 'data3'
		);
		$data = null;

		$closure = $this->Apc->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data', 'expiry');
		$result = $closure($this->Apc, $params, null);
		$this->assertEqual(array(), $result);

		$expected = array(
			'key1' => 'data1',
			'key2' => 'data2',
			'key3' => 'data3'
		);
		$result = apc_fetch(array_keys($key));
		$this->assertEqual($expected, $result);

		$result = apc_delete(array_keys($key));
		$this->assertEqual(array(), $result);
	}

	public function testReadKeyThatDoesNotExist() {
		$key = 'does_not_exist';
		$closure = $this->Apc->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Apc, $params, null);
		$this->assertFalse($result);
	}

	public function testDelete() {
		$key = 'delete_key';
		$data = 'data to delete';

		$result = apc_store($key, $data, 60);
		$this->assertTrue($result);

		$closure = $this->Apc->delete($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Apc, $params, null);
		$this->assertTrue($result);
	}

	public function testDeleteMulti() {
		$expiry = '+1 minute';
		$key = array(
			'key1' => 'data1',
			'key2' => 'data2',
			'key3' => 'data3'
		);
		$data = null;

		$closure = $this->Apc->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data', 'expiry');
		$result = $closure($this->Apc, $params, null);
		$this->assertEqual(array(), $result);

		$expected = array(
			'key1' => 'data1',
			'key2' => 'data2',
			'key3' => 'data3'
		);
		$result = apc_delete(array_keys($key));
		$this->assertEqual(array(), $result);
	}

	public function testDeleteNonExistentKey() {
		$key = 'delete_key';
		$data = 'data to delete';

		$closure = $this->Apc->delete($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Apc, $params, null);
		$this->assertFalse($result);
	}

	public function testWriteReadAndDeleteRoundtrip() {
		$key = 'write_read_key';
		$data = 'write/read value';
		$expiry = '+5 seconds';

		$closure = $this->Apc->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data', 'expiry');
		$result = $closure($this->Apc, $params, null);
		$expected = $data;
		$this->assertTrue($result);

		$result = apc_fetch($key);
		$this->assertEqual($expected, $result);

		$closure = $this->Apc->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Apc, $params, null);
		$expected = $data;
		$this->assertEqual($expected, $result);

		$closure = $this->Apc->delete($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Apc, $params, null);
		$this->assertTrue($result);
	}

	public function testClear() {
		$key1 = 'key_clear_1';
		$key2 = 'key_clear_2';

		$result = apc_store($key1, 'data that will no longer exist', 60);
		$this->assertTrue($result);

		$result = apc_store($key2, 'more dead data', 60);
		$this->assertTrue($result);

		$result = $this->Apc->clear();
		$this->assertTrue($result);

		$this->assertFalse(apc_fetch($key1));
		$this->assertFalse(apc_fetch($key2));
	}

	public function testDecrement() {
		$key = 'decrement';
		$value = 10;

		$result = apc_store($key, $value, 60);
		$this->assertTrue($result);

		$closure = $this->Apc->decrement($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Apc, $params, null);
		$this->assertEqual($value - 1, $result);

		$result = apc_fetch($key);
		$this->assertEqual($value - 1, $result);

		$result = apc_delete($key);
		$this->assertTrue($result);
	}

	public function testDecrementNonIntegerValue() {
		$key = 'non_integer';
		$value = 'no';

		$result = apc_store($key, $value, 60);
		$this->assertTrue($result);

		$closure = $this->Apc->decrement($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Apc, $params, null);

		$result = apc_fetch($key);
		$this->assertEqual('no', $result);

		$result = apc_delete($key);
		$this->assertTrue($result);
	}

	public function testIncrement() {
		$key = 'increment';
		$value = 10;

		$result = apc_store($key, $value, 60);
		$this->assertTrue($result);

		$closure = $this->Apc->increment($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Apc, $params, null);
		$this->assertEqual($value + 1, $result);

		$result = apc_fetch($key);
		$this->assertEqual($value + 1, $result);

		$result = apc_delete($key);
		$this->assertTrue($result);
	}

	public function testIncrementNonIntegerValue() {
		$key = 'non_integer_increment';
		$value = 'yes';

		$result = apc_store($key, $value, 60);
		$this->assertTrue($result);

		$closure = $this->Apc->increment($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->Apc, $params, null);

		$result = apc_fetch($key);
		$this->assertEqual('yes', $result);

		$result = apc_delete($key);
		$this->assertTrue($result);
	}
}

?>