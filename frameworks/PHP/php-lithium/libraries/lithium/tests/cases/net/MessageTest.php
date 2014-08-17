<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\net;

use lithium\net\Message;

class MessageTest extends \lithium\test\Unit {

	public $request = null;

	public function setUp() {
		$this->message = new Message();
	}

	public function testBody() {
		$expected = "Part 1";
		$result = $this->message->body('Part 1');
		$this->assertEqual($expected, $result);

		$expected = "Part 1\r\nPart 2";
		$result = $this->message->body('Part 2');
		$this->assertEqual($expected, $result);

		$expected = "Part 1\r\nPart 2\r\nPart 3\r\nPart 4";
		$result = $this->message->body(array('Part 3', 'Part 4'));
		$this->assertEqual($expected, $result);

		$expected = array('Part 1', 'Part 2', 'Part 3', 'Part 4');
		$result = $this->message->body;
		$this->assertEqual($expected, $result);
	}

	public function testBodyBuffer() {
		$expected = array('P', 'a', 'r', 't', ' ', '1');
		$result = $this->message->body('Part 1', array('buffer' => 1));
		$this->assertEqual($expected, $result);
	}

	public function testToArray() {
		$expected = array(
			'scheme' => 'tcp',
			'host' => 'localhost',
			'port' => null,
			'path' => null,
			'username' => null,
			'password' => null,
			'body' => array()
		);
		$result = $this->message->to('array');
		$this->assertEqual($expected, $result);
	}

	public function testToUrl() {
		$expected = "tcp://localhost";
		$result = $this->message->to('url');
		$this->assertEqual($expected, $result);
	}

	public function testToContext() {
		$expected = array('tcp' => array('content' => null, 'ignore_errors' => true));
		$result = $this->message->to('context');
		$this->assertEqual($expected, $result);
	}

	public function testToString() {
		$expected = "woohoo";
		$this->message->body($expected);
		$result = (string) $this->message;
		$this->assertEqual($expected, $result);

		$result = $this->message->to('string');
		$this->assertEqual($expected, $result);
	}

	public function testConstruct() {
		$expected = array(
			'scheme' => 'http',
			'host' => 'localhost',
			'port' => '80',
			'path' => null,
			'username' => null,
			'password' => null,
			'body' => array()
		);
		$message = new Message($expected);
		$result = $message->to('array');
		$this->assertEqual($expected, $result);
	}
}

?>