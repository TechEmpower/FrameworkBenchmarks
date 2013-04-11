<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\analysis\logger\adapter;

use lithium\analysis\logger\adapter\Growl;

class GrowlTest extends \lithium\test\Unit {

	public function testGrowlWrite() {
		$connection = fopen('php://memory', 'w+');

		$growl = new Growl(compact('connection') + array(
			'name' => 'Lithium',
			'title' => 'Lithium log'
		));
		$writer = $growl->write('info', 'info: Test message.', array());
		$params = array('message' => 'info: Test message.', 'options' => array());
		$result = $writer('lithium\analysis\Logger', $params, null);

		$bytes = array(
			1, 0, 0, 7, 2, 2, 76, 105, 116, 104, 105, 117, 109, 0, 6, 69, 114, 114, 111, 114, 115,
			0, 8, 77, 101, 115, 115, 97, 103, 101, 115, 0, 1, 126, 154, 165, 127, 162, 58, 0, 172,
			243, 11, 201, 119, 62, 33, 133, 55, 1, 1, 0, 0, 0, 8, 0, 11, 0, 19, 0, 7, 77, 101, 115,
			115, 97, 103, 101, 115, 76, 105, 116, 104, 105, 117, 109, 32, 108, 111, 103, 105, 110,
			102, 111, 58, 32, 84, 101, 115, 116, 32, 109, 101, 115, 115, 97, 103, 101, 46, 76, 105,
			116, 104, 105, 117, 109, 213, 182, 8, 47, 80, 71, 225, 173, 12, 228, 108, 152, 140, 126,
			102, 14
		);

		rewind($connection);
		$result = array_map('ord', str_split(stream_get_contents($connection)));
		$this->assertEqual($bytes, $result);
	}

	public function testInvalidConnection() {
		$growl = new Growl(array(
			'name' => 'Lithium',
			'title' => 'Lithium log',
			'port' => 0
		));
		$this->expectException('/^Growl connection failed/');
		$this->expectException('/Failed to parse address/');

		$message = 'info: Test message.';
		$params = compact('message') + array('priority' => 'info', 'options' => array());
		$writer = $growl->write('info', $message, array());
		$writer('lithium\analysis\Logger', $params, null);
	}

	public function testInvalidConnectionWithForcedRegistration() {
		$growl = new Growl(array(
			'name' => 'Lithium',
			'title' => 'Lithium log',
			'port' => 0,
			'registered' => true
		));
		$this->expectException('/^Growl connection failed/');
		$this->expectException('/Failed to parse address/');

		$message = 'info: Test message.';
		$params = compact('message') + array('priority' => 'info', 'options' => array());
		$writer = $growl->write('info', $message, array());
		$writer('lithium\analysis\Logger', $params, null);
	}

	public function testStickyMessages() {
		$connection = fopen('php://memory', 'w+');

		$growl = new Growl(compact('connection') + array(
			'name' => 'Lithium',
			'title' => 'Lithium log'
		));
		$writer = $growl->write('info', 'info: Test message.', array());
		$params = array('message' => 'info: Test message.', 'options' => array('sticky' => true));
		$result = $writer('lithium\analysis\Logger', $params, null);

		$bytes = array(
			1, 0, 0, 7, 2, 2, 76, 105, 116, 104, 105, 117, 109, 0, 6, 69, 114, 114, 111, 114, 115,
			0, 8, 77, 101, 115, 115, 97, 103, 101, 115, 0, 1, 126, 154, 165, 127, 162, 58, 0, 172,
			243, 11, 201, 119, 62, 33, 133, 55, 1, 1, 1, 0, 0, 8, 0, 11, 0, 19, 0, 7, 77, 101, 115,
			115, 97, 103, 101, 115, 76, 105, 116, 104, 105, 117, 109, 32, 108, 111, 103, 105, 110,
			102, 111, 58, 32, 84, 101, 115, 116, 32, 109, 101, 115, 115, 97, 103, 101, 46, 76, 105,
			116, 104, 105, 117, 109, 123, 79, 163, 67, 106, 115, 6, 31, 170, 247, 50, 98, 144, 44,
			105, 89
		);

		rewind($connection);
		$result = array_map('ord', str_split(stream_get_contents($connection)));
		$this->assertEqual($bytes, $result);
	}

	public function testMessagePriority() {
		$connection = fopen('php://memory', 'w+');

		$growl = new Growl(compact('connection') + array(
			'name' => 'Lithium',
			'title' => 'Lithium log'
		));
		$writer = $growl->write('info', 'info: Test message.', array());
		$params = array('message' => 'info: Test message.', 'options' => array(
			'priority' => 'emergency'
		));
		$result = $writer('lithium\analysis\Logger', $params, null);

		$bytes = array(
			1, 0, 0, 7, 2, 2, 76, 105, 116, 104, 105, 117, 109, 0, 6, 69, 114, 114, 111, 114, 115,
			0, 8, 77, 101, 115, 115, 97, 103, 101, 115, 0, 1, 126, 154, 165, 127, 162, 58, 0, 172,
			243, 11, 201, 119, 62, 33, 133, 55, 1, 1, 0, 4, 0, 8, 0, 11, 0, 19, 0, 7, 77, 101, 115,
			115, 97, 103, 101, 115, 76, 105, 116, 104, 105, 117, 109, 32, 108, 111, 103, 105, 110,
			102, 111, 58, 32, 84, 101, 115, 116, 32, 109, 101, 115, 115, 97, 103, 101, 46, 76, 105,
			116, 104, 105, 117, 109, 180, 219, 185, 111, 150, 248, 170, 144, 208, 88, 63, 48, 171,
			130, 209, 32
		);

		rewind($connection);
		$result = array_map('ord', str_split(stream_get_contents($connection)));
		$this->assertEqual($bytes, $result);
	}
}

?>