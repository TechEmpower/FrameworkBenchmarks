<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\net\http;

use lithium\net\http\Auth;

class AuthTest extends \lithium\test\Unit {

	public function testBasicEncode() {
		$username = 'gwoo';
		$password = 'li3';
		$response = base64_encode("{$username}:{$password}");
		$expected = compact('username', 'response');
		$result = Auth::encode($username, $password);
		$this->assertEqual($expected, $result);
	}


	public function testDigestEncode() {
		$username = 'gwoo';
		$password = 'li3';
		$nc = '00000001';
		$cnonce = md5(time());
		$user = md5("gwoo:app:li3");
		$nonce = "4bca0fbca7bd0:{$nc}:{$cnonce}:auth";
		$req = md5("GET:/http_auth");
		$response = md5("{$user}:{$nonce}:{$req}");

		$data = array(
			'realm' => 'app',
			'method' => 'GET',
			'uri' => '/http_auth',
			'qop' => 'auth',
			'nonce' => '4bca0fbca7bd0',
			'opaque' => 'd3fb67a7aa4d887ec4bf83040a820a46'
		);
		$expected = $data + compact('username', 'response', 'nc', 'cnonce');
		$result = Auth::encode($username, $password, $data);
		$this->assertEqual($expected, $result);
	}

	public function testBasicHeader() {
		$username = 'gwoo';
		$password = 'li3';
		$response = base64_encode("{$username}:{$password}");
		$data = Auth::encode($username, $password);
		$expected = "Basic " . $response;
		$result = Auth::header($data);
		$this->assertEqual($expected, $result);
	}

	public function testDigestHeader() {
		$username = 'gwoo';
		$password = 'li3';
		$nc = '00000001';
		$cnonce = md5(time());
		$user = md5("gwoo:app:li3");
		$nonce = "4bca0fbca7bd0:{$nc}:{$cnonce}:auth";
		$req = md5("GET:/http_auth");
		$hash = md5("{$user}:{$nonce}:{$req}");

		$data = array(
			'realm' => 'app',
			'method' => 'GET',
			'uri' => '/http_auth',
			'qop' => 'auth',
			'nonce' => '4bca0fbca7bd0',
			'opaque' => 'd3fb67a7aa4d887ec4bf83040a820a46'
		);
		$data = Auth::encode($username, $password, $data);
		$header = Auth::header($data);
		$this->assertPattern('/Digest/', $header);
		preg_match('/response="(.*?)"/', $header, $matches);
		list($match, $response) = $matches;

		$expected = $hash;
		$result = $response;
		$this->assertEqual($expected, $result);
	}

	public function testDecode() {
		$header = 'qop="auth",nonce="4bca0fbca7bd0",';
		$header .= 'nc="00000001",cnonce="95b2cd1e179bf5414e52ed62811481cf",';
		$header .= 'uri="/http_auth",realm="app",';
		$header .= 'opaque="d3fb67a7aa4d887ec4bf83040a820a46",username="gwoo",';
		$header .= 'response="04d7d878c67f289f37e553d2025e3a52"';

		$expected = array(
			'qop' => 'auth', 'nonce' => '4bca0fbca7bd0',
			'nc' => '00000001', 'cnonce' => '95b2cd1e179bf5414e52ed62811481cf',
			'uri' => '/http_auth', 'realm' => 'app',
			'opaque' => 'd3fb67a7aa4d887ec4bf83040a820a46', 'username' => 'gwoo',
			'response' => '04d7d878c67f289f37e553d2025e3a52'
		);
		$result = Auth::decode($header);
		$this->assertEqual($expected, $result);
	}
}

?>