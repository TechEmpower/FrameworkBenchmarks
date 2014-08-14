<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\source\http\adapter;

class MockSocket extends \lithium\net\Socket {

	protected $_data = null;

	public function open(array $options = array()) {
		parent::open($options);
		return true;
	}

	public function close() {
		return true;
	}

	public function eof() {
		return true;
	}

	public function read() {
		return join("\r\n", array(
			'HTTP/1.1 200 OK',
			'Header: Value',
			'Connection: close',
			'Content-Type: text/html;charset=UTF-8',
			'',
			json_encode($this->_data)
		));
	}

	public function write($data) {
		$url = $data->to('url');
		$data = array('ok' => true, 'id' => '12345', 'rev' => '1-2', 'body' => 'something');

		if (strpos($url, '_all_docs')) {
			$data = array('total_rows' => 3, 'offset' => 0, 'rows' => array(
				array('doc' => array(
					'_id' => 'a1', '_rev' => '1-1',
					'author' => 'author 1',
					'body' => 'body 1'
				)),
				array('doc' => array(
					'_id' => 'a2', '_rev' => '1-2',
					'author' => 'author 2',
					'body' => 'body 2'
				)),
				array('doc' => array(
					'_id' => 'a3', '_rev' => '1-3',
					'author' => 'author 3',
					'body' => 'body 3'
				))
			));
		} else if (strpos($url, 'lithium-test/_design/latest/_view/all')) {
			$data = array('total_rows' => 3, 'offset' => 0, 'rows' => array(
				array('value' => array(
					'_id' => 'a1', '_rev' => '1-1',
					'author' => 'author 1',
					'body' => 'body 1'
				)),
				array('value' => array(
					'_id' => 'a2', '_rev' => '1-2',
					'author' => 'author 2',
					'body' => 'body 2'
				)),
				array('value' => array(
					'_id' => 'a3', '_rev' => '1-3',
					'author' => 'author 3',
					'body' => 'body 3'
				))
			));
		} else if (strpos($url, 'lithium-test/12345?rev=1-1')) {
			$data = array(
				'ok' => true, '_id' => '12345', '_rev' => '1-1'
			);
		} else if (strpos($url, 'lithium-test/12345')) {
			$data = array(
				'_id' => '12345', '_rev' => '1-2', 'author' => 'author 1', 'body' => 'body 1'
			);
		}
		return $this->_data = $data;
	}

	public function timeout($time) {
		return true;
	}

	public function encoding($charset) {
		return true;
	}
}

?>