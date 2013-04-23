<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\net\http;

class MockSocket extends \lithium\net\Socket {

	public $data = null;

	public $configs = array();

	public function __construct(array $config = array()) {
		parent::__construct((array) $config);
	}

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
		if ($this->data->path === '/http_auth/') {
			if (is_array($this->data->auth)) {
				$request = $this->data->to('array');
				$data = $this->data->auth;
				$data['nc'] = '00000001';
				$data['cnonce'] = md5(time());
				$username = $this->data->username;
				$password = $this->data->password;
				$part1 = md5("{$username}:{$data['realm']}:{$password}");
				$part2 = "{$data['nonce']}:{$data['nc']}:{$data['cnonce']}:{$data['qop']}";
				$part3 = md5($this->data->method . ':' . $this->data->path);
				$hash = md5("{$part1}:{$part2}:{$part3}");
				preg_match('/response="(.*?)"/', $this->data->headers('Authorization'), $matches);
				list($match, $response) = $matches;

				if ($hash === $response) {
					return 'success';
				}
			}
			$header = 'Digest realm="app",qop="auth",nonce="4bca0fbca7bd0",';
			$header .= 'opaque="d3fb67a7aa4d887ec4bf83040a820a46";';
			$this->data->headers('WWW-Authenticate', $header);
			$status = "GET HTTP/1.1 401 Authorization Required";
			$response = array($status, join("\r\n", $this->data->headers()), "", "not authorized");
			return join("\r\n", $response);
		}
		return (string) $this->data;
	}

	public function write($data) {
		if (!is_object($data)) {
			$data = $this->_instance($this->_classes['request'], (array) $data + $this->_config);
		}
		$this->data = $data;
		return true;
	}

	public function timeout($time) {
		return true;
	}

	public function encoding($charset) {
		return true;
	}

	public function config() {
		return $this->_config;
	}
}

?>