<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\core;

use lithium\core\Environment;
use lithium\tests\mocks\core\MockRequest;

class EnvironmentTest extends \lithium\test\Unit {

	public function setUp() {
		Environment::reset();
	}

	/**
	 * Tests setting and getting current environment, and that invalid environments cannot be
	 * selected.
	 */
	public function testSetAndGetCurrentEnvironment() {
		Environment::set('production',  array('foo' => 'bar'));
		Environment::set('staging',     array('foo' => 'baz'));
		Environment::set('development', array('foo' => 'dib'));

		Environment::set('development');

		$this->assertEqual('development', Environment::get());
		$this->assertTrue(Environment::is('development'));
		$this->assertNull(Environment::get('doesNotExist'));

		$expected = array('foo' => 'dib');
		$config = Environment::get('development');
		$this->assertEqual($expected, $config);

		$foo = Environment::get('foo'); // returns 'dib', since the current env. is 'development'
		$expected = 'dib';
		$this->assertEqual($expected, $foo);
	}

	/**
	 * Tests that a set of configuration keys can be assigned to multiple environments.
	 */
	public function testSetMultipleEnvironments() {
		foreach (array('foo', 'bar', 'baz') as $key) {
			$this->assertNull(Environment::get($key));
		}

		Environment::set(array('foo', 'bar', 'baz'), array(
			'key' => array('subkey' => 'value')
		));

		foreach (array('foo', 'bar', 'baz') as $key) {
			$this->assertEqual(array('key' => array('subkey' => 'value')), Environment::get($key));
		}
	}

	/**
	 * Tests creating a custom environment, and verifies that settings are properly retrieved.
	 */
	public function testCreateNonStandardEnvironment() {
		Environment::set('custom', array('host' => 'server.local'));
		Environment::set('custom');

		$host = Environment::get('host');
		$expected = 'server.local';
		$this->assertEqual($expected, $host);

		$custom = Environment::get('custom');
		$expected = array('host' => 'server.local');
		$this->assertEqual($expected, $custom);
	}

	/**
	 * Tests modifying environment configuration.
	 */
	public function testModifyEnvironmentConfig() {
		Environment::set('test', array('foo' => 'bar'));
		$expected = array('foo' => 'bar');
		$this->assertEqual($expected, Environment::get('test'));

		$expected = array('foo' => 'bar', 'baz' => 'qux');
		Environment::set('test', array('baz' => 'qux'));
		$settings = Environment::get('test'); // returns array('foo' => 'bar', 'baz' => 'qux')
		$this->assertEqual($expected, Environment::get('test'));
	}

	/**
	 * Tests auto-detecting environment settings through a series of mock request classes.
	 */
	public function testEnvironmentDetection() {
		Environment::set(new MockRequest(array('SERVER_ADDR' => '::1')));
		$this->assertTrue(Environment::is('development'));

		$request = new MockRequest(array('SERVER_ADDR' => '1.1.1.1', 'HTTP_HOST' => 'test.local'));
		Environment::set($request);
		$this->assertTrue(Environment::is('test'));

		$request = new MockRequest(array('SERVER_ADDR' => '1.1.1.1', 'HTTP_HOST' => 'www.com'));
		Environment::set($request);
		$isProduction = Environment::is('production');
		$this->assertTrue($isProduction);

		$request = new MockRequest(array('SERVER_ADDR' => '::1'));
		$request->url = 'test/myTest';
		Environment::set($request);
		$this->assertTrue(Environment::is('test'));

		$request = new MockRequest();
		$request->command = 'test';
		Environment::set($request);
		$this->assertTrue(Environment::is('test'));

		$request = new MockRequest();
		$request->env = 'test';
		Environment::set($request);
		$this->assertTrue(Environment::is('test'));

		$request = new MockRequest(array('PLATFORM' => 'CLI'));
		Environment::set($request);
		$this->assertTrue(Environment::is('development'));

		$request = new MockRequest();
		$request->params = array('env' => 'production');
		Environment::set($request);
		$this->assertTrue(Environment::is('production'));
	}

	/**
	 * Tests that environment names can be mapped to lists of host names, or a hostname-matching
	 * regular expression.
	 */
	public function testDetectionWithArrayMap() {
		Environment::is(array(
			'development' => '/^local|^\.console/',
			'test' => array('test1.myapp.com', 'test2.myapp.com'),
			'staging' => array('staging.myapp.com')
		));

		Environment::set(new MockRequest(array('http:host' => 'localhost')));
		$this->assertTrue(Environment::is('development'));

		Environment::set(new MockRequest(array('http:host' => 'test1.myapp.com')));
		$this->assertTrue(Environment::is('test'));

		Environment::set(new MockRequest(array('http:host' => 'test3.myapp.com')));
		$this->assertTrue(Environment::is('production'));

		Environment::set(new MockRequest(array('http:host' => 'localhost:3030')));
		$this->assertTrue(Environment::is('development'));

		$request = new MockRequest();
		$request->params = array('env' => 'whatever');
		Environment::set($request);
		$this->assertTrue(Environment::is('whatever'));
	}

	/**
	 * Tests resetting the `Environment` class to its default state.
	 */
	public function testReset() {
		Environment::set('test', array('foo' => 'bar'));
		Environment::set('test');
		$this->assertEqual('test', Environment::get());
		$this->assertEqual('bar', Environment::get('foo'));

		Environment::reset();
		$this->assertEqual('', Environment::get());
		$this->assertNull(Environment::get('foo'));
	}

	/**
	 * Tests using a custom detector to get the current environment.
	 */
	public function testCustomDetector() {
		Environment::is(function($request) {
			if ($request->env('HTTP_HOST') === 'localhost') {
				return 'development';
			}
			if ($request->env('HTTP_HOST') === 'staging.server') {
				return 'test';
			}
			return 'production';
		});

		$request = new MockRequest(array('HTTP_HOST' => 'localhost'));
		Environment::set($request);
		$this->assertTrue(Environment::is('development'));

		$request = new MockRequest(array('HTTP_HOST' => 'lappy.local'));
		Environment::set($request);
		$this->assertTrue(Environment::is('production'));

		$request = new MockRequest(array('HTTP_HOST' => 'staging.server'));
		Environment::set($request);
		$this->assertTrue(Environment::is('test'));

		$request = new MockRequest(array('HTTP_HOST' => 'test.local'));
		Environment::set($request);
		$this->assertTrue(Environment::is('production'));
	}

	public function testDotPath() {
		$data = array(
			'foo' => array('bar' => array('baz' => 123)),
			'some' => array('path' => true),
			'string' => 'lorem ipsum'
		);
		Environment::set('dotPathIndex', $data);

		$this->assertEqual(123, Environment::get('dotPathIndex.foo.bar.baz'));
		$this->assertEqual($data['foo'], Environment::get('dotPathIndex.foo'));
		$this->assertTrue(Environment::get('dotPathIndex.some.path'));
		$this->assertFalse(Environment::get('dotPathIndex.string.b'));
	}

	/**
	 * Tests calling `get()` and `set()` with `true` as the envrionment name, to automatically
	 * select the current environment.
	 */
	public function testReadWriteWithDefaultEnvironment() {
		Environment::set('development');
		Environment::set(true, array('foo' => 'bar'));

		$this->assertEqual(array('foo' => 'bar'), Environment::get('development'));
		$this->assertEqual(Environment::get(true), Environment::get('development'));

		Environment::set('production');
		$this->assertFalse(Environment::get(true));
	}
}

?>