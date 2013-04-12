<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\analysis;

use lithium\core\Libraries;
use lithium\analysis\Logger;
use lithium\tests\mocks\analysis\MockLoggerAdapter;

/**
 * Logger adapter test case
 */
class LoggerTest extends \lithium\test\Unit {

	public function skip() {
		$path = Libraries::get(true, 'resources');

		if (is_writable($path)) {
			foreach (array("{$path}/tmp/tests", "{$path}/tmp/logs") as $dir) {
				if (!is_dir($dir)) {
					mkdir($dir, 0777, true);
				}
			}
		}
		$this->_testPath = "{$path}/tmp/tests";
		$this->skipIf(!is_writable($this->_testPath), "Path `{$this->_testPath}` is not writable.");
	}

	public function setUp() {
		Logger::config(array('default' => array('adapter' => new MockLoggerAdapter())));
	}

	public function tearDown() {
		Logger::reset();
	}

	public function testConfig() {
		$test = new MockLoggerAdapter();
		$config = array('logger' => array('adapter' => $test, 'filters' => array()));

		$result = Logger::config($config);
		$this->assertNull($result);

		$result = Logger::config();
		$config['logger'] += array('priority' => true);
		$expected = $config;
		$this->assertEqual($expected, $result);
	}

	public function testReset() {
		$test = new MockLoggerAdapter();
		$config = array('logger' => array('adapter' => $test, 'filters' => array()));

		$result = Logger::config($config);

		$result = Logger::reset();
		$this->assertNull($result);

		$result = Logger::config();
		$this->assertFalse($result);

		$this->assertFalse(Logger::write('info', 'Test message.'));
	}

	public function testWrite() {
		$result = Logger::write('info', 'value');
		$this->assertTrue($result);
	}

	public function testIntegrationWriteFile() {
		$base = Libraries::get(true, 'resources') . '/tmp/logs';
		$this->skipIf(!is_writable($base), "Path `{$base}` is not writable.");

		$config = array('default' => array(
			'adapter' => 'File', 'timestamp' => false, 'format' => "{:message}\n"
		));
		Logger::config($config);

		$result = Logger::write('info', 'Message line 1');
		$this->assertTrue(file_exists($base . '/info.log'));

		$expected = "Message line 1\n";
		$result = file_get_contents($base . '/info.log');
		$this->assertEqual($expected, $result);

		$result = Logger::write('info', 'Message line 2');
		$this->assertTrue($result);

		$expected = "Message line 1\nMessage line 2\n";
		$result = file_get_contents($base . '/info.log');
		$this->assertEqual($expected, $result);

		unlink($base . '/info.log');
	}

	public function testWriteWithInvalidPriority() {
		$this->expectException("Attempted to write log message with invalid priority `foo`.");
		Logger::foo("Test message");
	}

	public function testWriteByName() {
		$base = Libraries::get(true, 'resources') . '/tmp/logs';
		$this->skipIf(!is_writable($base), "Path `{$base}` is not writable.");

		Logger::config(array('default' => array(
			'adapter' => 'File',
			'timestamp' => false,
			'priority' => false,
			'format' => "{:message}\n"
		)));

		$this->assertFalse(file_exists($base . '/info.log'));

		$this->assertFalse(Logger::write('info', 'Message line 1'));
		$this->assertFalse(file_exists($base . '/info.log'));

		$this->assertTrue(Logger::write(null, 'Message line 1', array('name' => 'default')));

		$expected = "Message line 1\n";
		$result = file_get_contents($base . '/.log');
		$this->assertEqual($expected, $result);

		unlink($base . '/.log');
	}

	public function testMultipleAdaptersWriteByNameDefault() {
		$base = Libraries::get(true, 'resources') . '/tmp/logs';
		$this->skipIf(!is_writable($base), "Path `{$base}` is not writable.");

		Logger::config(array(
			'default' => array(
				'adapter' => 'File',
				'file' => function($data, $config) { return "{$data['priority']}_default.log"; },
				'timestamp' => false,
				'format' => "{:message}\n"
			),
			'secondary' => array(
				'adapter' => 'File',
				'file' => function($data, $config) { return "{$data['priority']}_secondary.log"; },
				'timestamp' => false,
				'format' => "{:message}\n"
			),
		));

		$this->assertFalse(file_exists($base . '/info_default.log'));

		$this->assertTrue(Logger::write('info', 'Default Message line 1', array(
			'name' => 'default'
		)));

		$this->assertTrue(file_exists($base . '/info_default.log'));

		$expected = "Default Message line 1\n";
		$result = file_get_contents($base . '/info_default.log');
		$this->assertEqual($expected, $result);

		unlink($base . '/info_default.log');

	}

	public function testMultipleAdaptersWriteByNameSecondary() {
		$base = Libraries::get(true, 'resources') . '/tmp/logs';
		$this->skipIf(!is_writable($base), "Path `{$base}` is not writable.");

		Logger::config(array(
			'default' => array(
				'adapter' => 'File',
				'file' => function($data, $config) { return "{$data['priority']}_default.log"; },
				'timestamp' => false,
				'format' => "{:message}\n"
			),
			'secondary' => array(
				'adapter' => 'File',
				'file' => function($data, $config) { return "{$data['priority']}_secondary.log"; },
				'timestamp' => false,
				'format' => "{:message}\n"
			),
		));

		$this->assertFalse(file_exists($base . '/info_secondary.log'));

		$this->assertTrue(Logger::write('info', 'Secondary Message line 1', array(
			'name' => 'secondary'
		)));

		$this->assertTrue(file_exists($base . '/info_secondary.log'));

		$expected = "Secondary Message line 1\n";
		$result = file_get_contents($base . '/info_secondary.log');
		$this->assertEqual($expected, $result);

		unlink($base . '/info_secondary.log');

	}

	public function testRespondsToParentCall() {
		$this->assertTrue(Logger::respondsTo('applyFilter'));
		$this->assertFalse(Logger::respondsTo('fooBarBaz'));
	}

	public function testRespondsToMagic() {
		$this->assertTrue(Logger::respondsTo('emergency'));
		$this->assertTrue(Logger::respondsTo('debug'));
		$this->assertFalse(Logger::respondsTo('foobar'));
	}

}

?>