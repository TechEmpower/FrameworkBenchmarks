<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\storage;

use lithium\storage\Session;
use lithium\storage\session\adapter\Memory;
use lithium\tests\mocks\storage\session\adapter\SessionStorageConditional;
use lithium\tests\mocks\storage\session\strategy\MockEncrypt;

/**
 *
 * @todo Refactor this to get rid of the very integration-style tests.
 */
class SessionTest extends \lithium\test\Unit {

	public function setUp() {
		Session::config(array(
			'default' => array('adapter' => new Memory())
		));
	}

	public function testSessionInitialization() {
		$store1 = new Memory();
		$store2 = new Memory();
		$config = array(
			'store1' => array('adapter' => &$store1, 'filters' => array()),
			'store2' => array('adapter' => &$store2, 'filters' => array())
		);

		Session::config($config);
		$result = Session::config();
		$this->assertEqual($config, $result);

		Session::reset();
		Session::config(array('store1' => array(
			'adapter' => 'lithium\storage\session\adapter\Memory',
			'filters' => array()
		)));
		$this->assertTrue(Session::write('key', 'value'));
		$result = Session::read('key');
		$expected = 'value';
		$this->assertEqual($expected, $result);
	}

	public function testSingleStoreReadWrite() {
		$this->assertNull(Session::read('key'));

		$this->assertTrue(Session::write('key', 'value'));
		$this->assertEqual(Session::read('key'), 'value');

		Session::reset();
		$this->assertNull(Session::read('key'));
		$this->assertIdentical(false, Session::write('key', 'value'));
	}

	public function testNamedConfigurationReadWrite() {
		$store1 = new Memory();
		$store2 = new Memory();
		$config = array(
			'store1' => array('adapter' => &$store1, 'filters' => array()),
			'store2' => array('adapter' => &$store2, 'filters' => array())
		);
		Session::reset();
		Session::config($config);
		$result = Session::config();
		$this->assertEqual($config, $result);

		$result = Session::write('key', 'value', array('name' => 'store1'));
		$this->assertTrue($result);

		$result = Session::read('key', array('name' => 'store1'));
		$this->assertEqual($result, 'value');

		$result = Session::read('key', array('name' => 'store2'));
		$this->assertFalse($result);
	}

	public function testSessionConfigReset() {
		$this->assertTrue(Session::write('key', 'value'));
		$this->assertEqual(Session::read('key'), 'value');

		Session::reset();
		$this->assertFalse(Session::config());

		$this->assertFalse(Session::read('key'));
		$this->assertFalse(Session::write('key', 'value'));
	}

	/**
	 * Tests a scenario where no session handler is available that matches the passed parameters.
	 *
	 * @return void
	 */
	public function testUnhandledWrite() {
		Session::config(array(
			'conditional' => array('adapter' => new SessionStorageConditional())
		));
		$result = Session::write('key', 'value', array('fail' => true));
		$this->assertFalse($result);
	}

	/**
	 * Tests deleting a session key from one or all adapters.
	 *
	 * @return void
	 */
	public function testSessionKeyCheckAndDelete() {
		Session::config(array(
			'temp' => array('adapter' => new Memory(), 'filters' => array()),
			'persistent' => array('adapter' => new Memory(), 'filters' => array())
		));
		Session::write('key1', 'value', array('name' => 'persistent'));
		Session::write('key2', 'value', array('name' => 'temp'));

		$this->assertTrue(Session::check('key1'));
		$this->assertTrue(Session::check('key2'));

		$this->assertTrue(Session::check('key1', array('name' => 'persistent')));
		$this->assertFalse(Session::check('key1', array('name' => 'temp')));

		$this->assertFalse(Session::check('key2', array('name' => 'persistent')));
		$this->assertTrue(Session::check('key2', array('name' => 'temp')));

		Session::delete('key1');
		$this->assertFalse(Session::check('key1'));

		Session::write('key1', 'value', array('name' => 'persistent'));
		$this->assertTrue(Session::check('key1'));

		Session::delete('key1', array('name' => 'temp'));
		$this->assertTrue(Session::check('key1'));

		Session::delete('key1', array('name' => 'persistent'));
		$this->assertFalse(Session::check('key1'));
	}

	/**
	 * Tests clearing all session data from one or all adapters.
	 *
	 * @return void
	 */
	public function testSessionClear() {
		Session::config(array(
			'primary' => array('adapter' => new Memory(), 'filters' => array()),
			'secondary' => array('adapter' => new Memory(), 'filters' => array())
		));
		Session::write('key1', 'value', array('name' => 'primary'));
		Session::write('key2', 'value', array('name' => 'secondary'));

		Session::clear(array('name' => 'secondary'));
		$this->assertTrue(Session::check('key1'));
		$this->assertFalse(Session::check('key2'));

		Session::write('key2', 'value', array('name' => 'secondary'));
		Session::clear();
		$this->assertFalse(Session::check('key1'));
		$this->assertFalse(Session::check('key2'));
	}

	/**
	 * Tests querying session keys from the primary adapter.
	 * The memory adapter returns a UUID.
	 *
	 * @return void
	 */

	public function testKey() {
		$result = Session::key();
		$pattern = "/^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/";
		$this->assertPattern($pattern, $result);
	}

	public function testConfigNoAdapters() {
		Session::config(array(
			'conditional' => array('adapter' => new SessionStorageConditional())
		));
		$this->assertTrue(Session::write('key', 'value'));
		$this->assertEqual(Session::read('key'), 'value');
		$this->assertFalse(Session::read('key', array('fail' => true)));
	}

	public function testSessionState() {
		$this->assertTrue(Session::isStarted());
		$this->assertTrue(Session::isStarted('default'));
		$this->expectException("Configuration `invalid` has not been defined.");
		$this->assertFalse(Session::isStarted('invalid'));
	}

	public function testSessionStateReset() {
		Session::reset();
		$this->assertFalse(Session::isStarted());
	}

	public function testSessionStateResetNamed() {
		Session::reset();
		$this->expectException("Configuration `default` has not been defined.");
		$this->assertFalse(Session::isStarted('default'));
	}

	public function testReadFilter() {
		Session::config(array(
			'primary' => array('adapter' => new Memory(), 'filters' => array()),
			'secondary' => array('adapter' => new Memory(), 'filters' => array())
		));
		Session::applyFilter('read', function($self, $params, $chain) {
			$result = $chain->next($self, $params, $chain);

			if (isset($params['options']['increment'])) {
				$result += $params['options']['increment'];
			}
			return $result;
		});
		Session::write('foo', 'bar');
		$this->assertEqual('bar', Session::read('foo'));

		Session::write('bar', 1);
		$this->assertEqual(2, Session::read('bar', array('increment' => 1)));
	}

	public function testStrategies() {
		Session::config(array('primary' => array(
			'adapter' => new Memory(), 'filters' => array(), 'strategies' => array(
				'lithium\storage\cache\strategy\Json'
			)
		)));

		Session::write('test', array('foo' => 'bar'));
		$this->assertEqual(array('foo' => 'bar'), Session::read('test'));

		$this->assertTrue(Session::check('test'));
		$this->assertTrue(Session::check('test', array('strategies' => false)));

		$result = Session::read('test', array('strategies' => false));
		$this->assertEqual('{"foo":"bar"}', $result);

		$result = Session::clear(array('strategies' => false));
		$this->assertNull(Session::read('test'));

		$this->assertFalse(Session::check('test'));
		$this->assertFalse(Session::check('test', array('strategies' => false)));
	}

	public function testMultipleStrategies() {
		Session::config(array(
			'primary' => array(
				'adapter' => new Memory(),
				'filters' => array(),
				'strategies' => array()
			),
			'secondary' => array(
				'adapter' => new Memory(),
				'filters' => array(),
				'strategies' => array('lithium\storage\cache\strategy\Json')
			)
		));

		Session::write('test', array('foo' => 'bar'));
		$result = Session::read('test');
		$this->assertEqual(array('foo' => 'bar'), $result);

		$result = Session::read('test', array('name' => 'primary', 'strategies' => false));
		$this->assertEqual(array('foo' => 'bar'), $result);

		$result = Session::read('test', array('name' => 'secondary', 'strategies' => false));
		$this->assertEqual('{"foo":"bar"}', $result);
	}

	public function testEncryptedStrategy() {
		$this->skipIf(!MockEncrypt::enabled(), 'The Mcrypt extension is not installed or enabled.');

		$key = 'foobar';
		$adapter = new Memory();
		Session::config(array('primary' => array(
			'adapter' => $adapter, 'filters' => array(), 'strategies' => array(
				'lithium\tests\mocks\storage\session\strategy\MockEncrypt' => array(
					'secret' => $key
				)
			)
		)));

		$value = array('foo' => 'bar');

		Session::write('test', $value);
		$this->assertEqual(array('foo' => 'bar'), Session::read('test'));

		$this->assertTrue(Session::check('test'));
		$this->assertTrue(Session::check('test', array('strategies' => false)));

		$encrypted = Session::read('test', array('strategies' => false));

		$this->assertNotEqual($value, $encrypted);
		$this->assertTrue(is_string($encrypted));

		$result = Session::read('test');
		$this->assertEqual($value, $result);

		$result = Session::clear(array('strategies' => false));
		$this->assertNull(Session::read('test'));

		$this->assertFalse(Session::check('test'));
		$this->assertFalse(Session::check('test', array('strategies' => false)));

		$savedData = array('test' => $value);

		$encrypt = new MockEncrypt(array('secret' => $key));
		$result = $encrypt->encrypt($savedData);
		$this->assertEqual($encrypted, $result);
		$result = $encrypt->decrypt($encrypted);
		$this->assertEqual($savedData, $result);
	}
}

?>