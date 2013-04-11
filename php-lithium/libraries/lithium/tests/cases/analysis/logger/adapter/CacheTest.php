<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright	 Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license	   http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\analysis\logger\adapter;

use lithium\storage\Cache As CacheStorage;
use lithium\analysis\Logger;
use lithium\analysis\logger\adapter\Cache;

/**
 * Tests the "Cache" logger adapter.
 */
class CacheTest extends \lithium\test\Unit {

	/**
	 * Sets up and configers the logger and also the cache storage for testing.
	 */
	public function setUp() {
		CacheStorage::config(array(
			'cachelog' => array(
				'adapter' => 'Memory'
			)
		));
		$this->cachelog = new Cache(array(
			'key' => 'cachelog_testkey',
			'config' => 'cachelog'
		));
		Logger::config(array(
			'cachelog' => array(
				'adapter' => $this->cachelog,
				'key' => 'cachelog_testkey',
				'config' => 'cachelog'
			)
		));
	}

	/**
	 * Test the initialization of the cache log adapter.
	 */
	public function testConstruct() {
		$expected = array(
			'config' => "cachelog",
			'expiry' => "+999 days",
			'key' => "cachelog_testkey",
			'init' => true
		);
		$result = $this->cachelog->_config;
		$this->assertEqual($expected, $result);
	}

	/**
	 * Test if the configuration is correctly set in the logger.
	 */
	public function testConfiguration() {
		$loggers = Logger::config();
		$result = isset($loggers['cachelog']);
		$this->assertTrue($result);
	}

	/**
	 * Tests the correct writing to the cache adapter. In this test we use the
	 * "Memory" cache adapter so that we can easily verify the written message.
	 */
	public function testWrite() {
		$message = "CacheLog test message...";
		$result = Logger::write('info', $message, array('name' => 'cachelog'));
		$this->assertTrue($result);
		$result = CacheStorage::read('cachelog', 'cachelog_testkey');
		$this->assertEqual($message, $result);
	}
}

?>