<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\storage\cache\adapter;

use Exception;
use Redis as RedisCore;
use lithium\storage\cache\adapter\Redis;

class RedisTest extends \lithium\test\Unit {

	public function __construct(array $config = array()) {
		$defaults = array(
			'host' => '127.0.0.1',
			'port' => 6379
		);
		parent::__construct($config + $defaults);
	}

	/**
	 * Skip the test if the Redis extension is unavailable.
	 *
	 * @return void
	 */
	public function skip() {
		$this->skipIf(!Redis::enabled(), 'The redis extension is not installed.');

		$redis = new RedisCore();
		$cfg = $this->_config;

		try {
			$redis->connect($cfg['host'], $cfg['port']);
		} catch (Exception $e) {
			$info = $redis->info();
			$msg = "redis-server does not appear to be running on {$cfg['host']}:{$cfg['port']}";
			$this->skipIf(!$info, $msg);
		}
		unset($redis);
	}

	public function setUp() {
		$this->_redis = new RedisCore();
		$this->_redis->connect($this->_config['host'], $this->_config['port']);
		$this->redis = new Redis();
	}

	public function tearDown() {
		$this->_redis->flushdb();
	}

	public function testEnabled() {
		$redis = $this->redis;
		$this->assertTrue($redis::enabled());
	}

	public function testInit() {
		$redis = new Redis();
		$this->assertTrue($redis->connection instanceof RedisCore);
	}

	public function testSimpleWrite() {
		$key = 'key';
		$data = 'value';
		$expiry = '+5 seconds';
		$time = strtotime($expiry);

		$closure = $this->redis->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data', 'expiry');
		$result = $closure($this->redis, $params, null);
		$expected = $data;
		$this->assertEqual($expected, $result);

		$result = $this->_redis->get($key);
		$this->assertEqual($expected, $result);

		$result = $this->_redis->ttl($key);
		$this->assertEqual($time - time(), $result);

		$result = $this->_redis->delete($key);
		$this->assertTrue($result);

		$key = 'another_key';
		$data = 'more_data';
		$expiry = '+1 minute';
		$time = strtotime($expiry);

		$closure = $this->redis->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data', 'expiry');
		$result = $closure($this->redis, $params, null);
		$expected = $data;
		$this->assertEqual($expected, $result);

		$result = $this->_redis->get($key);
		$this->assertEqual($expected, $result);

		$result = $this->_redis->ttl($key);
		$this->assertEqual($time - time(), $result);

		$result = $this->_redis->delete($key);
		$this->assertTrue($result);
	}

	public function testWriteDefaultCacheExpiry() {
		$redis = new Redis(array('expiry' => '+5 seconds'));
		$key = 'default_key';
		$data = 'value';
		$time = strtotime('+5 seconds');

		$closure = $redis->write($key, $data);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data');
		$result = $closure($redis, $params, null);
		$expected = $data;
		$this->assertEqual($expected, $result);

		$result = $this->_redis->get($key);
		$this->assertEqual($expected, $result);

		$result = $this->_redis->ttl($key);
		$this->assertEqual($time - time(), $result);

		$result = $this->_redis->delete($key);
		$this->assertTrue($result);
	}

	public function testWriteNoCacheExpiry() {
		$redis = new Redis(array('expiry' => null));
		$key = 'default_key';
		$data = 'value';
		$redis->write($key, $data)->__invoke(null, compact('key', 'data'), null);
		$this->assertEqual($data, $this->_redis->get($key));
		$this->assertTrue($this->_redis->delete($key));
	}

	public function testSimpleRead() {
		$key = 'read_key';
		$data = 'read data';

		$result = $this->_redis->set($key, $data);
		$this->assertTrue($result);

		$closure = $this->redis->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->redis, $params, null);
		$expected = $data;
		$this->assertEqual($expected, $result);

		$result = $this->_redis->delete($key);
		$this->assertTrue($result);

		$key = 'another_read_key';
		$data = 'read data';
		$time = strtotime('+1 minute');

		$result = $this->_redis->set($key, $data);
		$this->assertTrue($result);

		$result = $this->_redis->ttl($key);
		$this->assertTrue($result);

		$closure = $this->redis->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->redis, $params, null);
		$expected = $data;
		$this->assertEqual($expected, $result);

		$result = $this->_redis->delete($key);
		$this->assertTrue($result);
	}

	public function testMultiRead() {
		$data = array('key1' => 'value1', 'key2' => 'value2');
		$result = $this->_redis->mset($data);
		$this->assertTrue($result);

		$closure = $this->redis->read(array_keys($data));
		$this->assertTrue(is_callable($closure));

		$params = array('key' => array_keys($data));
		$result = $closure($this->redis, $params, null);
		$expected = array_values($data);
		$this->assertEqual($expected, $result);

		foreach ($data as $k => $v) {
			$result = $this->_redis->delete($k);
			$this->assertTrue($result);
		}
	}

	public function testMultiWrite() {
		$key = array('key1' => 'value1', 'key2' => 'value2');
		$expiry = '+5 seconds';
		$time = strtotime($expiry);

		$closure = $this->redis->write($key, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = array('key' => $key, 'data' => $expiry, 'expiry' => null);
		$result = $closure($this->redis, $params, null);
		$expected = array('key1' => true, 'key2' => true);
		$this->assertEqual($expected, $result);

		$result = $this->_redis->getMultiple(array_keys($key));
		$expected = array_values($key);
		$this->assertEqual($expected, $result);
	}

	public function testReadKeyThatDoesNotExist() {
		$key = 'does_not_exist';
		$closure = $this->redis->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->redis, $params, null);
		$this->assertFalse($result);

	}

	public function testDelete() {
		$key = 'delete_key';
		$data = 'data to delete';
		$time = strtotime('+1 minute');

		$result = $this->_redis->set($key, $data);
		$this->assertTrue($result);

		$closure = $this->redis->delete($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->redis, $params, null);
		$this->assertTrue($result);

		$this->assertFalse($this->_redis->delete($key));
	}

	public function testDeleteNonExistentKey() {
		$key = 'delete_key';
		$closure = $this->redis->delete($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->redis, $params, null);
		$this->assertFalse($result);
	}

	public function testWriteReadAndDeleteRoundtrip() {
		$key = 'write_read_key';
		$data = 'write/read value';
		$expiry = '+5 seconds';
		$time = strtotime($expiry);

		$closure = $this->redis->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data', 'expiry');
		$result = $closure($this->redis, $params, null);
		$expected = $data;
		$this->assertEqual($expected, $result);

		$result = $this->_redis->get($key);
		$this->assertEqual($expected, $result);

		$closure = $this->redis->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->redis, $params, null);
		$expected = $data;
		$this->assertEqual($expected, $result);

		$closure = $this->redis->delete($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->redis, $params, null);
		$this->assertTrue($result);

		$this->assertFalse($this->_redis->get($key));
	}

	public function testClear() {
		$result = $this->_redis->set('key', 'value');
		$this->assertTrue($result);

		$result = $this->_redis->set('another_key', 'value');
		$this->assertTrue($result);

		$result = $this->redis->clear();
		$this->assertTrue($result);

		$this->assertFalse($this->_redis->get('key'));
		$this->assertFalse($this->_redis->get('another_key'));
	}

	public function testDecrement() {
		$key = 'decrement';
		$value = 10;

		$result = $this->_redis->set($key, $value);
		$this->assertTrue($result);

		$closure = $this->redis->decrement($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->redis, $params, null);
		$this->assertEqual($value - 1, $result);

		$result = $this->_redis->get($key);
		$this->assertEqual($value - 1, $result);

		$result = $this->_redis->delete($key);
		$this->assertTrue($result);
	}

	public function testDecrementNonIntegerValue() {
		$key = 'non_integer';
		$value = 'no';

		$result = $this->_redis->set($key, $value);
		$this->assertTrue($result);

		$closure = $this->redis->decrement($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->redis, $params, null);
		$this->assertFalse($result);

		$result = $this->_redis->get($key);
		$this->assertEqual($value, $result);

		$result = $this->_redis->delete($key);
		$this->assertTrue($result);
	}

	public function testIncrement() {
		$key = 'increment';
		$value = 10;

		$result = $this->_redis->set($key, $value);
		$this->assertTrue($result);

		$closure = $this->redis->increment($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->redis, $params, null);
		$this->assertEqual($value + 1, $result);

		$result = $this->_redis->get($key);
		$this->assertEqual($value + 1, $result);

		$result = $this->_redis->delete($key);
		$this->assertTrue($result);
	}

	public function testIncrementNonIntegerValue() {
		$key = 'non_integer_increment';
		$value = 'yes';

		$result = $this->_redis->set($key, $value);
		$this->assertTrue($result);

		$closure = $this->redis->increment($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->redis, $params, null);
		$this->assertFalse($result);

		$result = $this->_redis->get($key);
		$this->assertEqual($value, $result);

		$result = $this->_redis->delete($key);
		$this->assertTrue($result);
	}

	public function testMethodDispatch() {

		$this->_redis->flushdb();
		$this->_redis->set('some_key', 'somevalue');

		$result = $this->redis->keys('*');
		$this->assertEqual($result, array('some_key'), 'redis method dispatch failed');

		$result = $this->redis->info();
		$this->assertTrue(is_array($result), 'redis method dispatch failed');
	}

	public function testRespondsTo() {
		$this->assertTrue($this->redis->respondsTo('bgsave'));
		$this->assertTrue($this->redis->respondsTo('dbSize'));
		$this->assertFalse($this->redis->respondsTo('foobarbaz'));
	}

	public function testRespondsToParentCall() {
		$this->assertTrue($this->redis->respondsTo('applyFilter'));
		$this->assertFalse($this->redis->respondsTo('fooBarBaz'));
	}

}

?>