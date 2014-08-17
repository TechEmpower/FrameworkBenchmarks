<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of Rad, Inc. (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\storage\session\adapter;

use lithium\core\Libraries;
use lithium\storage\session\adapter\Php;
use lithium\tests\mocks\storage\session\adapter\MockPhp;

class PhpTest extends \lithium\test\Unit {

	public function setUp() {
		$this->_session = isset($_SESSION) ? $_SESSION : array();
		$this->_destroySession();

		$this->php = new Php();
		$this->_destroySession();

		/* Garbage collection */
		$this->_gc_divisor = ini_get('session.gc_divisor');
		ini_set('session.gc_divisor', '1');
	}

	public function tearDown() {
		$this->_destroySession();

		/* Revert to original garbage collection probability */
		ini_set('session.gc_divisor', $this->_gc_divisor);
		$_SESSION = $this->_session;
	}

	protected function _destroySession($name = null) {
		if (!$name) {
			$name = session_name();
		}
		$settings = session_get_cookie_params();
		setcookie(
			$name, '', time() - 1000, $settings['path'], $settings['domain'],
			$settings['secure'], $settings['httponly']
		);
		if (session_id()) {
			session_destroy();
		}
		$_SESSION = array();
	}

	public function testEnabled() {
		$php = $this->php;
		$this->_destroySession(session_name());
		$this->assertFalse($php::enabled());
	}

	public function testInit() {
		$id = session_id();
		$this->assertTrue(empty($id));

		$result = ini_get('session.name');
		$this->assertEqual(basename(LITHIUM_APP_PATH), $result);

		$result = ini_get('session.cookie_lifetime');
		$this->assertEqual(0, (integer) $result);

		$result = ini_get('session.cookie_httponly');
		$this->assertTrue(1, (integer) $result);

		$name = 'this-is-a-custom-name';
		$php = new Php(array('session.name' => $name));
		$this->assertFalse(is_numeric($php->_config['session.name']));
	}

	public function testCustomConfiguration() {
		$config = array(
			'session.name' => 'awesome_name', 'session.cookie_lifetime' => 1200,
			'session.cookie_domain' => 'awesome.domain',
			'session.save_path' => Libraries::get(true, 'resources') . '/tmp/',
			'somebad.configuration' => 'whoops'
		);

		$adapter = new Php($config);

		$result = ini_get('session.name');
		$this->assertEqual($config['session.name'], $result);

		$result = ini_get('session.cookie_lifetime');
		$this->assertEqual($config['session.cookie_lifetime'], (integer) $result);

		$result = ini_get('session.cookie_domain');
		$this->assertEqual($config['session.cookie_domain'], $result);

		$result = ini_get('session.cookie_secure');
		$this->assertFalse($result);

		$result = ini_get('session.cookie_httponly');
		$this->assertTrue($result);

		$result = ini_get('session.save_path');
		$this->assertEqual($config['session.save_path'], $result);

		$result = ini_get('somebad.configuration');
		$this->assertFalse($result);
	}

	public function testIsStarted() {
		$result = $this->php->isStarted();
		$this->assertFalse($result);

		$this->php->read();

		$result = $this->php->isStarted();
		$this->assertTrue($result);

		$this->_destroySession(session_name());
		$result = $this->php->isStarted();
		$this->assertFalse($result);
	}

	public function testIsStartedNoInit() {
		$this->_destroySession(session_name());

		$php = new Php(array('init' => false));
		$result = $php->isStarted();
		$this->assertFalse($result);

		$php = new Php();
		$php->read();
		$result = $php->isStarted();
		$this->assertTrue($result);
	}

	public function testKey() {
		$result = $this->php->key();
		$this->assertEqual(session_id(), $result);

		$this->_destroySession(session_name());
		$result = $this->php->key();
		$this->assertNull($result);
	}

	public function testWrite() {
		$key = 'write-test';
		$value = 'value to be written';

		$closure = $this->php->write($key, $value);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'value');
		$result = $closure($this->php, $params, null);

		$this->assertEqual($_SESSION[$key], $value);
	}

	public function testRead() {
		$this->php->read();

		$key = 'read_test';
		$value = 'value to be read';

		$_SESSION[$key] = $value;

		$closure = $this->php->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->php, $params, null);

		$this->assertIdentical($value, $result);

		$key = 'non-existent';
		$closure = $this->php->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->php, $params, null);
		$this->assertNull($result);

		$closure = $this->php->read();
		$this->assertTrue(is_callable($closure));

		$result = $closure($this->php, array('key' => null), null);
		$expected = array('read_test' => 'value to be read');
		$this->assertEqual($expected, $result);
	}

	public function testCheck() {
		$this->php->read();

		$key = 'read';
		$value = 'value to be read';
		$_SESSION[$key] = $value;

		$closure = $this->php->check($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->php, $params, null);
		$this->assertTrue($result);

		$key = 'does_not_exist';
		$closure = $this->php->check($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->php, $params, null);
		$this->assertFalse($result);
	}

	public function testDelete() {
		$this->php->read();

		$key = 'delete_test';
		$value = 'value to be deleted';

		$_SESSION[$key] = $value;

		$closure = $this->php->delete($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->php, $params, null);
		$this->assertTrue($result);

		$key = 'non-existent';
		$closure = $this->php->delete($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->php, $params, null);
		$this->assertTrue($result);
	}

	/**
	 * Checks if erasing the whole session array works as expected.
	 */
	public function testClear() {
		$_SESSION['foo'] = 'bar';
		$this->assertFalse(empty($_SESSION));
		$closure = $this->php->clear();
		$this->assertTrue(is_callable($closure));
		$result = $closure($this->php, array(), null);
		$this->assertTrue(empty($_SESSION));
	}

	public function testCheckThrowException() {
		$php = new MockPhp(array('init' => false));
		$this->expectException('/Could not start session./');
		$php->check('whatever');
	}

	public function testReadThrowException() {
		$php = new MockPhp(array('init' => false));
		$this->expectException('/Could not start session./');
		$php->read('whatever');
	}

	public function testWriteThrowException() {
		$php = new MockPhp(array('init' => false));
		$this->expectException('/Could not start session./');
		$php->write('whatever', 'value');
	}

	public function testDeleteThrowException() {
		$php = new MockPhp(array('init' => false));
		$this->expectException('/Could not start session./');
		$php->delete('whatever');
	}

	public function testReadDotSyntax() {
		$this->php->read();

		$key = 'dot';
		$value = array('syntax' => array('key' => 'value'));

		$_SESSION[$key] = $value;

		$closure = $this->php->read($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$result = $closure($this->php, $params, null);

		$this->assertIdentical($value, $result);

		$params = array('key' => 'dot.syntax');
		$result = $closure($this->php, $params, null);

		$this->assertIdentical($value['syntax'], $result);
	}

	public function testWriteDotSyntax() {
		$key = 'dot.syntax';
		$value = 'value to be written';

		$closure = $this->php->write($key, $value);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'value');
		$result = $closure($this->php, $params, null);

		$this->assertEqual($_SESSION['dot']['syntax'], $value);
	}
}

?>