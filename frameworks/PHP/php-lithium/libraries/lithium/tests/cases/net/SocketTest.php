<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\net;

use lithium\tests\mocks\net\http\MockSocket;
use lithium\net\http\Request;
use lithium\net\http\Response;

class SocketTest extends \lithium\test\Unit {

	public function testInitialization() {
		$socket = new MockSocket();
		$socket->open(array('test' => true));
		$config = $socket->config();
		$this->assertTrue($config['test']);
	}

	public function testSend() {
		$socket = new MockSocket();
		$message = new Request();
		$response = $socket->send($message, array('response' => 'lithium\net\http\Response'));
		$this->assertTrue($response instanceof Response);
		$this->assertTrue($socket->data instanceof Request);
	}
}

?>