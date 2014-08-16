<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\net\http;

use lithium\net\http\Request;

class RequestTest extends \lithium\test\Unit {

	public $request = null;

	public function setUp() {
		$this->request = new Request(array('init' => false));
	}

	public function testConstruct() {
		$request = new Request(array(
			'host' => 'localhost',
			'port' => 443,
			'headers' => array('Header' => 'Value'),
			'body' => array('Part 1')
		));

		$expected = 'localhost';
		$result = $request->host;
		$this->assertEqual($expected, $result);

		$expected = 443;
		$result = $request->port;
		$this->assertEqual($expected, $result);

		$expected = 'GET';
		$result = $request->method;
		$this->assertEqual($expected, $result);

		$expected = 'HTTP/1.1';
		$result = $request->protocol;
		$this->assertEqual($expected, $result);

		$expected = '1.1';
		$result = $request->version;
		$this->assertEqual($expected, $result);

		$expected = '/';
		$result = $request->path;
		$this->assertEqual($expected, $result);

		$expected = array(
			'Host: localhost:443',
			'Connection: Close',
			'User-Agent: Mozilla/5.0',
			'Header: Value'
		);
		$result = $request->headers();
		$this->assertEqual($expected, $result);

		$expected = array();
		$result = $request->cookies;
		$this->assertEqual($expected, $result);

		$expected = 'Part 1';
		$result = $request->body();
		$this->assertEqual($expected, $result);
	}

	public function testConstructWithPath() {
		$request = new Request(array(
			'host' => 'localhost/base/path',
			'port' => 443,
			'headers' => array('Header' => 'Value'),
			'body' => array('Part 1'),
			'params' => array('param' => 'value')
		));

		$expected = '/base/path';
		$result = $request->path;
		$this->assertEqual($expected, $result);
	}

	public function testQueryStringDefault() {
		$expected = "?param=value&param1=value1";
		$result = $this->request->queryString(array('param' => 'value', 'param1' => 'value1'));
		$this->assertEqual($expected, $result);
	}

	public function testQueryStringFormat() {
		$expected = "?param:value;param1:value1";
		$result = $this->request->queryString(
			array('param' => 'value', 'param1' => 'value1'), "{:key}:{:value};"
		);
		$this->assertEqual($expected, $result);
	}

	public function testQueryStringSetup() {
		$expected = "?param=value";
		$result = $this->request->queryString(array('param' => 'value'));
		$this->assertEqual($expected, $result);

		$expected = "?param=value";
		$this->request->query = array('param' => 'value');
		$result = $this->request->queryString();
		$this->assertEqual($expected, $result);

		$expected = "?param=value&param2=value2";
		$result = $this->request->queryString(array('param2' => 'value2'));
		$this->assertEqual($expected, $result);
	}

	public function testToString() {
		$expected = join("\r\n", array(
			'GET / HTTP/1.1',
			'Host: localhost',
			'Connection: Close',
			'User-Agent: Mozilla/5.0',
			'', ''
		));
		$result = (string) $this->request;
		$this->assertEqual($expected, $result);

		$result = $this->request->to('string');
		$this->assertEqual($expected, $result);
	}

	public function testToStringWithAuth() {
		$request = new Request(array(
			'auth' => 'Basic',
			'username' => 'root',
			'password' => 'something'
		));
		$expected = join("\r\n", array(
			'GET / HTTP/1.1',
			'Host: localhost',
			'Connection: Close',
			'User-Agent: Mozilla/5.0',
			'Authorization: Basic ' . base64_encode('root:something'),
			'', ''
		));
		$result = (string) $request;
		$this->assertEqual($expected, $result);
	}

	public function testToContextWithAuth() {
		$request = new Request(array(
			'auth' => 'Basic',
			'username' => 'Aladdin',
			'password' => 'open sesame'
		));
		$expected = array('http' => array(
			'method' => 'GET',
			'header' => array(
				'Host: localhost',
				'Connection: Close',
				'User-Agent: Mozilla/5.0',
				'Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=='
			),
			'content' => '',
			'protocol_version' => '1.1',
			'ignore_errors' => true,
			'follow_location' => true,
			'request_fulluri' => false,
			'proxy' => null
		));
		$this->assertEqual($expected, $request->to('context'));
	}

	public function testToStringWithBody() {
		$expected = join("\r\n", array(
			'GET / HTTP/1.1',
			'Host: localhost',
			'Connection: Close',
			'User-Agent: Mozilla/5.0',
			'Content-Length: 11',
			'', 'status=cool'
		));
		$this->request->body(array('status=cool'));
		$result = (string) $this->request;
		$this->assertEqual($expected, $result);
	}

	public function testToArray() {
		$expected = array(
			'method' => 'GET',
			'query' => array(),
			'headers' => array(
				'Host' => 'localhost',
				'Connection' => 'Close',
				'User-Agent' => 'Mozilla/5.0'
			),
			'cookies' => array(),
			'protocol' => 'HTTP/1.1',
			'version' => '1.1',
			'body' => array(),
			'scheme' => 'http',
			'host' => 'localhost',
			'port' => null,
			'path' => '/',
			'auth' => null,
			'username' => null,
			'password' => null
		);
		$result = $this->request->to('array');
		$this->assertEqual($expected, $result);
	}

	/**
	 * Tests that creating a `Request` with a proxy configuration correctly modifies the results
	 * of exporting the `Request` to a stream context configuration.
	 */
	public function testWithProxy() {
		$request = new Request(array('proxy' => 'tcp://proxy.example.com:5100'));
		$expected = array('http' => array(
			'content' => '',
			'method' => 'GET',
			'header' => array('Host: localhost', 'Connection: Close', 'User-Agent: Mozilla/5.0'),
			'protocol_version' => '1.1',
			'ignore_errors' => true,
			'follow_location' => true,
			'request_fulluri' => true,
			'proxy' => 'tcp://proxy.example.com:5100'
		));
		$this->assertEqual($expected, $request->to('context'));
	}

	public function testToUrl() {
		$expected = 'http://localhost/';
		$result = $this->request->to('url');
		$this->assertEqual($expected, $result);

		$this->request = new Request(array('scheme' => 'https', 'port' => 443));
		$expected = 'https://localhost:443/';
		$result = $this->request->to('url');
		$this->assertEqual($expected, $result);
	}

	public function testToContext() {
		$expected = array('http' => array(
			'method' => 'GET',
			'content' => '',
			'header' => array(
				'Host: localhost',
				'Connection: Close',
				'User-Agent: Mozilla/5.0'
			),
			'protocol_version' => '1.1',
			'ignore_errors' => true,
			'follow_location' => true,
			'request_fulluri' => false,
			'proxy' => null
		));
		$result = $this->request->to('context');
		$this->assertEqual($expected, $result);
	}

	public function testQueryStringWithArrayValues() {
		$expected = "?param%5B0%5D=value1&param%5B1%5D=value2";
		$result = $this->request->queryString(array('param' => array('value1', 'value2')));
		$this->assertEqual($expected, $result);
	}

	public function testQueryStringWithArrayValuesCustomFormat() {
		$expected = "?param%5B%5D:value1/param%5B%5D:value2";
		$result = $this->request->queryString(
			array('param' => array('value1', 'value2')),
			"{:key}:{:value}/"
		);
		$this->assertEqual($expected, $result);
	}

	public function testDigest() {
		$request = new Request(array(
			'path' => '/http_auth',
			'auth' => array(
				'realm' => 'app',
				'qop' => 'auth',
				'nonce' => '4bca0fbca7bd0',
				'opaque' => 'd3fb67a7aa4d887ec4bf83040a820a46'
			),
			'username' => 'gwoo',
			'password' => 'li3'
		));
		$cnonce = md5(time());
		$user = md5("gwoo:app:li3");
		$nonce = "4bca0fbca7bd0:00000001:{$cnonce}:auth";
		$req = md5("GET:/http_auth");
		$hash = md5("{$user}:{$nonce}:{$req}");

		$request->to('url');
		preg_match('/response="(.*?)"/', $request->headers('Authorization'), $matches);
		list($match, $response) = $matches;

		$expected = $hash;
		$result = $response;
		$this->assertEqual($expected, $result);
	}

	public function testParseUrlToConfig() {
		$url = "http://localhost/path/one.php?param=1&param=2";
		$config = parse_url($url);
		$request = new Request($config);

		$expected = $url;
		$result = $request->to('url');
		$this->assertEqual($expected, $result);
	}

	public function testQueryParamsConstructed() {
		$url = "http://localhost/path/one.php?param=1&param=2";
		$config = parse_url($url);
		$request = new Request($config);

		$expected = "?param=1&param=2";
		$result = $request->queryString();
		$this->assertEqual($expected, $result);

		$expected = "?param=1&param=2&param3=3";
		$result = $request->queryString(array('param3' => 3));
		$this->assertEqual($expected, $result);
	}

	public function testKeepDefinedContentTypeHeaderOnPost() {
		$request = new Request(array(
			'method' => 'POST',
			'headers' => array('Content-Type' => 'text/x-test')
		));
		$expected = 'Content-Type: text/x-test';
		$result = $request->headers();
		$message = "Expected value `{$expected}` not found in result.";
		$this->assertTrue(in_array($expected, $result), $message);

		$expected = '#Content-Type: text/x-test#';
		$result = $request->to('string');
		$this->assertPattern($expected, $result);
	}

	public function testKeepDefinedContentTypeHeaderWhenTypeIsSet() {
		$request = new Request(array(
			'method' => 'POST',
			'type' => 'json',
			'headers' => array('Content-Type' => 'text/x-test')
		));
		$expected = 'Content-Type: text/x-test';
		$result = $request->headers();
		$message = "Expected value `{$expected}` not found in result.";
		$this->assertTrue(in_array($expected, $result), $message);

		$expected = '#Content-Type: text/x-test#';
		$result = $request->to('string');
		$this->assertPattern($expected, $result);
	}
}

?>