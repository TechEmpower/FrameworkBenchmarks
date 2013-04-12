<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\integration\storage;

use lithium\storage\Session;

class SessionTest extends \lithium\test\Integration {

	public function skip() {
		$this->skipIf(PHP_SAPI === 'cli', 'No session support in cli SAPI');
	}

	public function tearDown() {
		Session::clear();
	}

	public function testPhpReadWriteDelete() {
		$config = array('name' => 'phpInt');

		Session::config(array(
			$config['name'] => array(
				'adapter' => 'Php'
			)
		));

		Session::clear($config);

		$key1 = 'key_one';
		$value1 = 'value_one';
		$key2 = 'key_two';
		$value2 = 'value_two';

		$this->assertNull(Session::read($key1, $config));
		$this->assertTrue(Session::write($key1, $value1, $config));
		$this->assertEqual($value1, Session::read($key1, $config));
		$this->assertNull(Session::read($key2, $config));
		$this->assertTrue(Session::delete($key1, $config));
		$this->assertNull(Session::read($key1, $config));
	}

	public function testCookieReadWriteDelete() {
		$config = array('name' => 'cookieInt');

		Session::config(array(
			$config['name'] => array(
				'adapter' => 'Cookie'
			)
		));

		Session::clear($config);

		$key1 = 'key_one';
		$value1 = 'value_one';
		$key2 = 'key_two';
		$value2 = 'value_two';

		$this->assertNull(Session::read($key1, $config));
		$this->assertTrue(Session::write($key1, $value1, $config));
		$this->assertCookie(array('key' => $key1, 'value' => $value1));
		$this->assertNull(Session::read($key2, $config));
		$this->assertTrue(Session::delete($key1, $config));
		$this->assertCookie(array('key' => $key1, 'value' => 'deleted'));
		$this->assertNoCookie(array('key' => $key2, 'value' => $value2));
		$this->assertNull(Session::read($key1, $config));
	}

	public function testMemoryReadWriteDelete() {
		$config = array('name' => 'memoryInt');

		Session::config(array(
			$config['name'] => array(
				'adapter' => 'Memory'
			)
		));

		Session::clear($config);

		$key1 = 'key_one';
		$value1 = 'value_one';
		$key2 = 'key_two';
		$value2 = 'value_two';

		$this->assertNull(Session::read($key1, $config));
		$this->assertTrue(Session::write($key1, $value1, $config));
		$this->assertEqual($value1, Session::read($key1, $config));
		$this->assertNull(Session::read($key2, $config));
		$this->assertTrue(Session::delete($key1, $config));
		$this->assertNull(Session::read($key1, $config));
	}

	public function testNamespacesWithPhpAdapter() {
		$config = array('name' => 'namespaceInt');

		Session::config(array(
			$config['name'] => array(
				'adapter' => 'Php'
			)
		));

		Session::clear($config);

		$key1 = 'really.deep.nested.key';
		$value1 = 'nested_val';
		$key2 = 'shallow.key';
		$value2 = 'shallow_val';

		$this->assertTrue(Session::write($key1, $value1, $config));
		$this->assertTrue(Session::write($key2, $value2, $config));
		$this->assertEqual($value1, Session::read($key1, $config));
		$this->assertEqual($value2, Session::read($key2, $config));
		$expected = array('nested' => array('key' => $value1));
		$this->assertEqual($expected, Session::read('really.deep', $config));
	}

	public function testHmacStrategyWithPhpAdapter() {
		$config = array('name' => 'hmacInt');

		Session::config(array(
			$config['name'] => array(
				'adapter' => 'Php',
				'strategies' => array(
					'Hmac' => array(
						'secret' => 's3cr3t'
					)
				)
			)
		));

		Session::clear($config);

		$key = 'test';
		$value = 'value';

		$this->assertTrue(Session::write($key, $value, $config));
		$this->assertEqual($value, Session::read($key, $config));
		$this->assertTrue(Session::delete($key, $config));
		$this->assertNull(Session::read($key, $config));

		Session::clear($config);

		$this->assertTrue(Session::write('foo', 'bar', $config));
		$this->assertEqual('bar', Session::read('foo', $config));
		$this->assertTrue(Session::write('foo', 'bar1', $config));
		$this->assertEqual('bar1', Session::read('foo', $config));

		Session::clear($config);

		$this->assertTrue(Session::write($key, $value, $config));
		$this->assertEqual($value, Session::read($key, $config));

		$cache = $_SESSION;
		$_SESSION['injectedkey'] = 'hax0r';
		$this->expectException('/Possible data tampering: HMAC signature does not match data./');
		Session::read($key, $config);
		$_SESSION = $cache;
	}

	public function testEncryptStrategyWithPhpAdapter() {
		$config = array('name' => 'encryptInt');

		Session::config(array(
			$config['name'] => array(
				'adapter' => 'Php',
				'strategies' => array(
					'Encrypt' => array(
						'secret' => 's3cr3t'
					)
				)
			)
		));

		Session::clear($config);

		$key = 'test';
		$value = 'value';

		$this->assertTrue(Session::write($key, $value, $config));
		$this->assertEqual($value, Session::read($key, $config));
		$this->assertTrue(Session::delete($key, $config));
		$this->assertNull(Session::read($key, $config));

		Session::clear($config);

		$this->assertTrue(Session::write('foo', 'bar', $config));
		$this->assertEqual('bar', Session::read('foo', $config));
		$this->assertTrue(Session::write('foo', 'bar1', $config));
		$this->assertEqual('bar1', Session::read('foo', $config));

		Session::clear($config);

		$this->assertTrue(Session::write($key, $value, $config));
		$this->assertEqual($value, Session::read($key, $config));
	}
}

?>