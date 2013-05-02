<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\net\socket;

use lithium\net\http\Response;
use lithium\net\http\Request;
use lithium\net\socket\Curl;

class CurlTest extends \lithium\test\Unit {

	protected $_testConfig = array(
		'persistent' => false,
		'scheme' => 'http',
		'host' => 'google.com',
		'port' => 80,
		'timeout' => 2,
		'classes' => array('request' => 'lithium\net\http\Request')
	);

	/**
	 * Skip the test if curl is not available in your PHP installation.
	 *
	 * @return void
	 */
	public function skip() {
		$message = 'Your PHP installation was not compiled with curl support.';
		$this->skipIf(!function_exists('curl_init'), $message);

		$config = $this->_testConfig;
		$url = "{$config['scheme']}://{$config['host']}";
		$message = "Could not open {$url} - skipping " . __CLASS__;
		$this->skipIf(!curl_init($url), $message);

		$message = "No internet connection established.";
		$this->skipIf(!$this->_hasNetwork($this->_testConfig), $message);
	}

	public function testAllMethodsNoConnection() {
		$stream = new Curl(array('scheme' => null));
		$this->assertFalse($stream->open());
		$this->assertTrue($stream->close());
		$this->assertFalse($stream->timeout(2));
		$this->assertFalse($stream->encoding('UTF-8'));
		$this->assertFalse($stream->write(null));
		$this->assertFalse($stream->read());
	}

	public function testOpen() {
		$stream = new Curl($this->_testConfig);
		$result = $stream->open();
		$this->assertTrue($result);

		$result = $stream->resource();
		$this->assertTrue(is_resource($result));
	}

	public function testClose() {
		$stream = new Curl($this->_testConfig);
		$result = $stream->open();
		$this->assertTrue($result);

		$result = $stream->close();
		$this->assertTrue($result);

		$result = $stream->resource();
		$this->assertFalse(is_resource($result));
	}

	public function testTimeout() {
		$stream = new Curl($this->_testConfig);
		$result = $stream->open();
		$stream->timeout(10);
		$result = $stream->resource();
		$this->assertTrue(is_resource($result));
	}

	public function testEncoding() {
		$stream = new Curl($this->_testConfig);
		$result = $stream->open();
		$stream->encoding('UTF-8');
		$result = $stream->resource();
		$this->assertTrue(is_resource($result));

		$stream = new Curl($this->_testConfig + array('encoding' => 'UTF-8'));
		$result = $stream->open();
		$result = $stream->resource();
		$this->assertTrue(is_resource($result));
	}

	public function testWriteAndRead() {
		$stream = new Curl($this->_testConfig);
		$this->assertTrue(is_resource($stream->open()));
		$this->assertTrue(is_resource($stream->resource()));
		$this->assertEqual(1, $stream->write());
		$this->assertPattern("/^HTTP/", (string) $stream->read());
	}

	public function testSendWithNull() {
		$stream = new Curl($this->_testConfig);
		$this->assertTrue(is_resource($stream->open()));
		$result = $stream->send(
			new Request($this->_testConfig),
			array('response' => 'lithium\net\http\Response')
		);
		$this->assertTrue($result instanceof Response);
		$this->assertPattern("/^HTTP/", (string) $result);
	}

	public function testSendWithArray() {
		$stream = new Curl($this->_testConfig);
		$this->assertTrue(is_resource($stream->open()));
		$result = $stream->send($this->_testConfig,
			array('response' => 'lithium\net\http\Response')
		);
		$this->assertTrue($result instanceof Response);
		$this->assertPattern("/^HTTP/", (string) $result);
	}

	public function testSendWithObject() {
		$stream = new Curl($this->_testConfig);
		$this->assertTrue(is_resource($stream->open()));
		$result = $stream->send(
			new Request($this->_testConfig),
			array('response' => 'lithium\net\http\Response')
		);
		$this->assertTrue($result instanceof Response);
		$this->assertPattern("/^HTTP/", (string) $result);
	}

	public function testSettingOfOptions() {
		$stream = new Curl($this->_testConfig);
		$stream->set('DummyFlag', 'Dummy Value');
		$stream->set('DummyFlag', 'Changed Dummy Value');
		$this->assertEqual('Changed Dummy Value', $stream->options['DummyFlag']);
	}

	public function testSettingOfOptionsInConfig() {
		$config = $this->_testConfig + array('options' => array('DummyFlag' => 'Dummy Value'));
		$stream = new Curl($config);
		$stream->open();
		$this->assertEqual('Dummy Value', $stream->options['DummyFlag']);
	}

	public function testSettingOfOptionsInOpen() {
		$stream = new Curl($this->_testConfig);
		$stream->open(array('options' => array('DummyFlag' => 'Dummy Value')));
		$this->assertEqual('Dummy Value', $stream->options['DummyFlag']);
	}

	public function testSendPostThenGet() {
		$postConfig = array('method' => 'POST', 'body' => '{"body"}');
		$stream = new Curl($this->_testConfig);
		$this->assertTrue(is_resource($stream->open()));
		$this->assertTrue($stream->write(new Request($postConfig + $this->_testConfig)));
		$this->assertTrue(isset($stream->options[CURLOPT_POST]));
		$this->assertTrue($stream->close());

		$this->assertTrue(is_resource($stream->open()));
		$this->assertTrue($stream->write(new Request($this->_testConfig)));
		$this->assertFalse(isset($stream->options[CURLOPT_POST]));
		$this->assertTrue($stream->close());
	}

	public function testSendPutThenGet() {
		$postConfig = array('method' => 'PUT', 'body' => '{"body"}');
		$stream = new Curl($this->_testConfig);
		$this->assertTrue(is_resource($stream->open()));
		$this->assertTrue($stream->write(new Request($postConfig + $this->_testConfig)));
		$this->assertTrue(isset($stream->options[CURLOPT_CUSTOMREQUEST]));
		$this->assertEqual($stream->options[CURLOPT_CUSTOMREQUEST],'PUT');
		$this->assertTrue(isset($stream->options[CURLOPT_POSTFIELDS]));
		$this->assertEqual($stream->options[CURLOPT_POSTFIELDS],$postConfig['body']);
		$this->assertTrue($stream->close());

		$this->assertTrue(is_resource($stream->open()));
		$this->assertTrue($stream->write(new Request($this->_testConfig)));
		$this->assertFalse(isset($stream->options[CURLOPT_CUSTOMREQUEST]));
		$this->assertTrue($stream->close());
	}
}

?>