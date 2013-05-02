<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\integration\net;

use lithium\net\socket\Curl;
use lithium\net\http\Response;
use lithium\net\socket\Stream;
use lithium\net\socket\Context;

class SocketTest extends \lithium\test\Integration {

	protected $_testConfig = array(
		'persistent' => false,
		'scheme' => 'http',
		'host' => 'google.com',
		'port' => 80,
		'timeout' => 1,
		'classes' => array(
			'request' => 'lithium\net\http\Request',
			'response' => 'lithium\net\http\Response'
		)
	);

	public function skip() {
		$message = "No internet connection established.";
		$this->skipIf(!$this->_hasNetwork($this->_testConfig), $message);
	}

	public function testContextAdapter() {
		$socket = new Context($this->_testConfig);
		$this->assertTrue($socket->open());
		$response = $socket->send();
		$this->assertTrue($response instanceof Response);

		$expected = 'google.com';
		$result = $response->host;
		$this->assertEqual($expected, $result);

		$result = $response->body();
		$this->assertPattern("/<title[^>]*>.*Google.*<\/title>/im", (string) $result);
	}

	public function testCurlAdapter() {
		$message = 'Your PHP installation was not compiled with curl support.';
		$this->skipIf(!function_exists('curl_init'), $message);

		$socket = new Curl($this->_testConfig);
		$this->assertTrue($socket->open());
		$response = $socket->send();
		$this->assertTrue($response instanceof Response);

		$expected = 'google.com';
		$result = $response->host;
		$this->assertEqual($expected, $result);

		$result = $response->body();
		$this->assertPattern("/<title[^>]*>.*<\/title>/im", (string) $result);
	}

	public function testStreamAdapter() {
		$socket = new Stream($this->_testConfig);
		$this->assertTrue($socket->open());
		$response = $socket->send();
		$this->assertTrue($response instanceof Response);

		$expected = 'google.com';
		$result = $response->host;
		$this->assertEqual($expected, $result);

		$result = $response->body();
		$this->assertPattern("/<title[^>]*>.*<\/title>/im", (string) $result);
	}
}

?>