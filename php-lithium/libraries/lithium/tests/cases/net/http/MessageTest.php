<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\net\http;

use lithium\net\http\Message;

class MessageTest extends \lithium\test\Unit {

	public $request = null;

	public function setUp() {
		$this->message = new Message();
	}

	public function testHeaderKey() {
		$expected = array('Host: localhost:80');
		$result = $this->message->headers('Host: localhost:80');
		$this->assertEqual($expected, $result);

		$expected = 'localhost:80';
		$result = $this->message->headers('Host');
		$this->assertEqual($expected, $result);

		$result = $this->message->headers('Host', false);
		$this->assertFalse($result);
	}

	public function testHeaderKeyValue() {
		$expected = array('Connection: Close');
		$result = $this->message->headers('Connection', 'Close');
		$this->assertEqual($expected, $result);
	}

	public function testHeaderArrayValue() {
		$expected = array('User-Agent: Mozilla/5.0');
		$result = $this->message->headers(array('User-Agent: Mozilla/5.0'));
		$this->assertEqual($expected, $result);
	}

	public function testHeaderArrayKeyValue() {
		$expected = array('Cache-Control: no-cache');
		$result = $this->message->headers(array('Cache-Control' => 'no-cache'));
		$this->assertEqual($expected, $result);
	}

	public function testMultiValueHeader() {
		$expected = array(
			'Cache-Control: no-store, no-cache, must-revalidate',
			'Cache-Control: post-check=0, pre-check=0',
			'Cache-Control: max-age=0'
		);
		$result = $this->message->headers(array(
			'Cache-Control' => array(
				'no-store, no-cache, must-revalidate',
				'post-check=0, pre-check=0',
				'max-age=0'
			)
		));
		$this->assertEqual($expected, $result);
	}

	public function testType() {
		$this->assertEqual('json', $this->message->type("json"));
		$this->assertEqual('json', $this->message->type());

		$expected = 'json';
		$result = $this->message->type("application/json; charset=UTF-8");
		$this->assertEqual($expected, $result);
	}

	public function testReturnStringIfNoBufferAndEmptyBody() {
		$this->message->type("json");
		$result = $this->message->body(array(""), array('encode' => true));
		$this->assertIdentical("", $result);
	}
}

?>