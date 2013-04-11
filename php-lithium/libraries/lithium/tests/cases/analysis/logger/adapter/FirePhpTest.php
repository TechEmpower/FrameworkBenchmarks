<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\analysis\logger\adapter;

use lithium\analysis\Logger;
use lithium\analysis\logger\adapter\FirePhp;
use lithium\action\Response;

/**
 * This tests make sure that the FirePhp log adapter works as expected.
 */
class FirePhpTest extends \lithium\test\Unit {

	/**
	 * Sets up and configures the logger and also the cache storage for testing.
	 */
	public function setUp() {
		$this->firephp = new FirePhp();
		Logger::config(array('firephp' => array('adapter' => $this->firephp)));
	}

	/**
	 * Test the initialization of the FirePhp log adapter.
	 */
	public function testConstruct() {
		$expected = array('init' => true);
		$this->assertEqual($expected, $this->firephp->_config);
	}

	/**
	 * Test if the configuration is correctly set in the logger.
	 */
	public function testConfiguration() {
		$loggers = Logger::config();
		$result = isset($loggers['firephp']);
		$this->assertTrue($result);
	}

	/**
	 * Tests the writing mechanism. At first, no Response object is bound to the logger, so
	 * it queues up the messages. When the Response object finally gets bound, it flushes the
	 * needed headers and all messages at once. All messages coming after this point get added
	 * to the header immediately.
	 */
	public function testWrite() {
		$response = new Response();
		$result = Logger::write('debug', 'FirePhp to the rescue!', array('name' => 'firephp'));
		$this->assertTrue($result);
		$this->assertFalse($response->headers());

		$host = 'meta.firephp.org';
		$expected = array(
			"X-Wf-Protocol-1: http://meta.wildfirehq.org/Protocol/JsonStream/0.2",
			"X-Wf-1-Plugin-1: http://{$host}/Wildfire/Plugin/FirePHP/Library-FirePHPCore/0.3",
			"X-Wf-1-Structure-1: http://{$host}/Wildfire/Structure/FirePHP/FirebugConsole/0.1",
			"X-Wf-1-1-1-1: 41|[{\"Type\":\"LOG\"},\"FirePhp to the rescue!\"]|"
		);
		Logger::adapter('firephp')->bind($response);
		$this->assertEqual($expected, $response->headers());

		$result = Logger::write('debug', 'Add this immediately.', array('name' => 'firephp'));
		$this->assertTrue($result);
		$expected[] = 'X-Wf-1-1-1-2: 40|[{"Type":"LOG"},"Add this immediately."]|';
		$this->assertEqual($expected, $response->headers());
	}
}

?>