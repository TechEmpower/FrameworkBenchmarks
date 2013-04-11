<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\integration\net\http;

use lithium\net\http\Service;

class ServiceTest extends \lithium\test\Integration {

	public function testStreamGet() {
		$service = new Service(array(
			'classes' => array('socket' => 'lithium\net\socket\Stream')
		));
		$service->head();

		$expected = array('code' => 200, 'message' => 'OK');
		$result = $service->last->response->status;
		$this->assertEqual($expected, $result);
	}

	public function testContextGet() {
		$service = new Service(array(
			'classes' => array('socket' => 'lithium\net\socket\Context')
		));
		$service->head();

		$expected = array('code' => 200, 'message' => 'OK');
		$result = $service->last->response->status;
		$this->assertEqual($expected, $result);
	}

	public function testCurlGet() {
		$service = new Service(array(
			'classes' => array('socket' => 'lithium\net\socket\Curl')
		));
		$service->head();

		$expected = array('code' => 200, 'message' => 'OK');
		$result = $service->last->response->status;
		$this->assertEqual($expected, $result);
	}
}

?>