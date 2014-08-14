<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\action;

use lithium\action\Request;
use lithium\tests\mocks\action\MockIisRequest;
use lithium\tests\mocks\action\MockNginxRequest;
use lithium\tests\mocks\action\MockCgiRequest;

class RequestTest extends \lithium\test\Unit {

	public $request = null;

	protected $_server = array();

	protected $_env = array();

	public function setUp() {
		$this->_server = $_SERVER;
		$this->_env = $_ENV;
		$this->request = new Request(array('init' => false));
	}

	public function tearDown() {
		$_SERVER = $this->_server;
		$_ENV = $this->_env;
		unset($this->request);
	}

	public function testInitData() {
		$_POST['Article']['title'] = 'cool';
		$request = new Request();

		$expected = array('Article' => array('title' => 'cool'));
		$result = $request->data;
		$this->assertEqual($expected, $result);

		unset($_POST, $request);
	}

	public function testInitMethodOverride() {
		$_POST['Article']['title'] = 'cool';
		$request = new Request(array('env' => array('HTTP_X_HTTP_METHOD_OVERRIDE' => 'GET')));

		$this->assertEqual('GET', $request->env('REQUEST_METHOD'));
		$this->assertEqual(array('Article' => array('title' => 'cool')), $request->data);
		unset($_POST);
	}

	public function testInitMethodOverrideWithEmptyServer() {
		$_POST['Article']['title'] = 'cool';
		$request = new Request(array('env' => array('HTTP_X_HTTP_METHOD_OVERRIDE' => 'POST')));
		$this->assertEqual('POST', $request->env('REQUEST_METHOD'));
		$this->assertEqual(array('Article' => array('title' => 'cool')), $request->data);
		unset($_POST['Article']);
	}

	public function testScriptFilename() {
		$request = new Request(array('env' => array(
			'SCRIPT_FILENAME' => '/lithium/app/webroot/index.php'
		)));
		$result = $request->env('SCRIPT_FILENAME');
		$this->assertEqual('/lithium/app/webroot/index.php', $result);
	}

	public function testPlatform() {
		$request = new MockIisRequest();
		$result = $request->env('PLATFORM');
		$this->assertEqual('IIS', $result);
	}

	public function testScriptFilenameTranslatedForIIS() {
		$request = new MockIisRequest();
		$this->assertEqual('\\lithium\\app\\webroot\\index.php', $request->env('SCRIPT_FILENAME'));

		$request = new Request(array('env' => array('SCRIPT_FILENAME' => null)));
		$path = $request->env('DOCUMENT_ROOT') . $request->env('PHP_SELF');
		$this->assertEqual($path, $request->env('SCRIPT_FILENAME'));
	}

	public function testDocumentRoot() {
		$request = new Request(array(
			'env' => array('DOCUMENT_ROOT' => '/home/lithium/app/webroot')
		));
		$this->assertEqual('/home/lithium/app/webroot', $request->env('DOCUMENT_ROOT'));
	}

	public function testDocumentRootTranslatedForIIS() {
		$request = new MockIisRequest();

		$expected = '\\lithium\\app\\webroot';
		$result = $request->env('DOCUMENT_ROOT');
		$this->assertEqual($expected, $result);
	}

	public function testScriptName() {
		$request = new Request(array(
			'env' => array('HTTPS' => true, 'SCRIPT_NAME' => 'index.php')
		));
		$this->assertEqual('index.php', $request->env('SCRIPT_NAME'));
	}

	public function testHttps() {
		$request = new Request(array('env' => array('HTTPS' => true)));
		$this->assertTrue($request->env('HTTPS'));
	}

	public function testHttpsFromScriptUri() {
		$request = new Request(array('env' => array(
			'SCRIPT_URI' => 'https://lithium.com',
			'HTTPS' => null
		)));
		$this->assertTrue($request->env('HTTPS'));
	}

	public function testRemoteAddr() {
		$request = new Request(array('env' => array('REMOTE_ADDR' => '123.456.789.000')));
		$this->assertEqual('123.456.789.000', $request->env('REMOTE_ADDR'));

		$request = new Request(array('env' => array(
			'REMOTE_ADDR' => '123.456.789.000',
			'HTTP_X_FORWARDED_FOR' => '111.222.333.444'
		)));
		$this->assertEqual('111.222.333.444', $request->env('REMOTE_ADDR'));

		$request = new Request(array('env' => array(
			'REMOTE_ADDR' => '123.456.789.000',
			'HTTP_PC_REMOTE_ADDR' => '222.333.444.555'
		)));
		$this->assertEqual('222.333.444.555', $request->env('REMOTE_ADDR'));

		$request = new Request(array('env' => array(
			'REMOTE_ADDR' => '123.456.789.000',
			'HTTP_X_REAL_IP' => '111.222.333.444'
		)));
		$this->assertEqual('111.222.333.444', $request->env('REMOTE_ADDR'));

		$request = new Request(array('env' => array(
			'REMOTE_ADDR' => '123.456.789.000',
			'HTTP_X_FORWARDED_FOR' => '111.222.333.444',
			'HTTP_PC_REMOTE_ADDR' => '222.333.444.555'
		)));
		$this->assertEqual('111.222.333.444', $request->env('REMOTE_ADDR'));
	}

	public function testRemoteAddrFromHttpPcRemoteAddr() {
		$request = new MockIisRequest();
		$this->assertEqual('123.456.789.000', $request->env('REMOTE_ADDR'));
	}

	public function testBase() {
		$request = new Request(array('env' => array('PHP_SELF' => '/index.php')));
		$this->assertFalse($request->env('base'));
	}

	public function testBaseWithDirectory() {
		$request = new Request(array('env' => array(
			'PHP_SELF' => '/lithium.com/app/webroot/index.php'
		)));
		$this->assertEqual('/lithium.com', $request->env('base'));
	}

	public function testRequestWithoutUrlQueryParam() {
		unset($_GET['url']);
		$request = new Request(array('env' => array(
			'PHP_SELF' => '/test_app/app/webroot/index.php',
			'REQUEST_URI' => '/test_app/'
		)));
		$this->assertEqual('/test_app', $request->env('base'));
		$this->assertEqual('/', $request->url);

		$request = new Request(array('env' => array(
			'PHP_SELF' => '/test_app/app/webroot/index.php',
			'REQUEST_URI' => '/test_app/pages/test_app'
		)));
		$this->assertEqual('/test_app', $request->env('base'));
		$this->assertEqual('pages/test_app', $request->url);
	}

	public function testRequestWithColon() {
		unset($_GET['url']);
		$request = new Request(array('env' => array(
			'PHP_SELF' => '/test_app/app/webroot/index.php',
			'REQUEST_URI' => '/test_app/pages/test_app/test:a'
		)));
		$this->assertEqual('/test_app', $request->env('base'));
		$this->assertEqual('pages/test_app/test:a', $request->url);

		$request = new Request(array('env' => array(
			'PHP_SELF' => '/test_app/app/webroot/index.php',
			'REQUEST_URI' => '/test_app/pages/test_app/test:1'
		)));
		$this->assertEqual('/test_app', $request->env('base'));
		$this->assertEqual('pages/test_app/test:1', $request->url);
	}

	public function testRequestWithoutUrlQueryParamAndNoApp() {
		unset($_GET['url']);
		$request = new Request(array('env' => array(
			'PHP_SELF' => '/test_app/webroot/index.php',
			'REQUEST_URI' => '/test_app/'
		)));
		$this->assertEqual('/test_app', $request->env('base'));
		$this->assertEqual('/', $request->url);
	}

	public function testRequestWithoutUrlQueryParamAndNoAppOrWebroot() {
		unset($_GET['url']);
		$request = new Request(array('env' => array(
			'PHP_SELF' => '/test_app/index.php',
			'REQUEST_URI' => '/test_app/'
		)));
		$this->assertEqual('/test_app', $request->env('base'));
		$this->assertEqual('/', $request->url);
	}

	public function testBaseWithAppAndOtherDirectory() {
		$request = new Request(array('env' => array(
			'PHP_SELF' => '/lithium.com/app/other/webroot/index.php'
		)));
		$this->assertEqual('/lithium.com/app/other', $request->env('base'));
	}

	public function testPhpSelfTranslatedForIIS() {
		$request = new MockIisRequest();
		$this->assertEqual('/index.php', $request->env('PHP_SELF'));
	}

	public function testServerHttpBase() {
		$_SERVER['HTTP_HOST'] = 'sub.lithium.local';
		$request = new Request();

		$expected = '.lithium.local';
		$result = $request->env('HTTP_BASE');
		$this->assertEqual($expected, $result);
	}

	public function testCgiPlatform() {
		$request = new MockCgiRequest();

		$result = $request->env('CGI_MODE');
		$this->assertTrue($result);
	}

	public function testCgiScriptUrl() {
		$request = new MockCgiRequest();

		$expected = '/lithium/app/webroot/index.php';
		$result = $request->env('SCRIPT_NAME');
		$this->assertEqual($expected, $result);
	}

	public function testGetMethod() {
		$request = new Request(array('env' => array(
			'PHP_SELF' => '/lithium.com/app/webroot/index.php',
			'HTTP_ACCEPT' => 'text/html,application/xml,image/png,*/*',
			'HTTP_ACCEPT_LANGUAGE' => 'da, en-gb;q=0.8, en;q=0.7'
		)));
		$request->data = array('Article' => array('title' => 'cool'));

		$expected = array('title' => 'cool');
		$result = $request->get('data:Article');
		$this->assertEqual($expected, $result);

		$result = $request->get('not:Post');
		$this->assertNull($result);

		$expected = '/lithium.com';
		$result = $request->get('env:base');
		$this->assertEqual($expected, $result);

		$accept = $request->get('http:accept');
		$this->assertEqual('text/html,application/xml,image/png,*/*', $accept);
		$this->assertEqual($request->get('http:method'), $request->env('REQUEST_METHOD'));
	}

	public function testDetect() {
		$request = new Request(array('env' => array('SOME_COOL_DETECTION' => true)));
		$request->detect('cool', 'SOME_COOL_DETECTION');

		$this->assertTrue($request->is('cool'));
		$this->assertFalse($request->is('foo'));

		$request = new Request(array('env' => array(
			'HTTP_USER_AGENT' => 'Mozilla/5.0 (iPhone; U; XXXXX like Mac OS X; en) AppleWebKit/420+'
		)));

		$request->detect('iPhone', array('HTTP_USER_AGENT', '/iPhone/'));
		$isiPhone = $request->is('iPhone'); // returns true if 'iPhone' appears anywhere in the UA
		$this->assertTrue($isiPhone);
	}

	public function testDetectWithClosure() {
		$request = new Request();
		$request->detect('cool', function ($self) { return true; });
		$request->detect('notCool', function ($self) { return false; });

		$this->assertTrue($request->is('cool'));
		$this->assertFalse($request->is('notCool'));
	}

	public function testDetectWithArray() {
		$request = new Request();
		$request->detect(array('cool' => function ($self) {
			return true;
		}));

		$result = $request->is('cool');
		$this->assertTrue($result);
	}

	public function testDetectWithArrayRegex() {
		$request = new Request(array('env' => array('SOME_COOL_DETECTION' => 'this is cool')));
		$request->detect('cool', array('SOME_COOL_DETECTION', '/cool/'));

		$result = $request->is('cool');
		$this->assertTrue($result);
	}

	public function testDetectSsl() {
		$request = new Request(array('env' => array('SCRIPT_URI' => null, 'HTTPS' => 'off')));
		$this->assertFalse($request->env('HTTPS'));

		$request = new Request(array('env' => array('SCRIPT_URI' => null, 'HTTPS' => 'on')));
		$this->assertTrue($request->env('HTTPS'));

		$request = new Request(array('env' => array('SCRIPT_URI' => null, 'HTTPS' => null)));
		$this->assertFalse($request->env('HTTPS'));
	}

	public function testContentTypeDetection() {
		$request = new Request(array('env' => array(
			'CONTENT_TYPE' => 'application/json; charset=UTF-8',
			'REQUEST_METHOD' => 'POST'
		)));
		$this->assertTrue($request->is('json'));
		$this->assertFalse($request->is('html'));
		$this->assertFalse($request->is('foo'));
	}

	public function testIsMobile() {
		$iPhone = 'Mozilla/5.0 (iPhone; U; CPU like Mac OS X; en) AppleWebKit/420+ (KHTML, like ';
		$iPhone .= 'Gecko) Version/3.0 Mobile/1A535b Safari/419.3';

		$request = new Request(array('env' => array('HTTP_USER_AGENT' => $iPhone)));
		$this->assertTrue($request->is('mobile'));

		$android = 'Mozilla/5.0 (Linux; U; Android 0.5; en-us) AppleWebKit/522+ (KHTML, like ';
		$android .= 'Gecko) Safari/419.3';

		$request = new Request(array('env' => array('HTTP_USER_AGENT' => $android)));
		$this->assertTrue($request->is('mobile'));
	}

	public function testType() {
		$request = new Request();
		$this->assertEqual('html', $request->type());

		$request = new Request(array('env' => array(
			'CONTENT_TYPE' => 'application/json; charset=UTF-8',
			'REQUEST_METHOD' => 'POST'
		)));
		$this->assertEqual('application/json; charset=UTF-8', $request->env('CONTENT_TYPE'));
		$this->assertEqual('json', $request->type());
	}

	public function testTypeforNginx() {
		$request = new MockNginxRequest();

		$this->assertEqual('html', $request->type());
	}

	public function testRefererDefault() {
		$_SERVER['HTTP_REFERER'] = null;
		$request = new Request();

		$expected = '/';
		$result = $request->referer('/');
		$this->assertEqual($expected, $result);
	}

	public function testRefererNotLocal() {
		$_SERVER['HTTP_REFERER'] = 'http://lithium.com/posts/index';
		$request = new Request();

		$expected = 'http://lithium.com/posts/index';
		$result = $request->referer('/');
		$this->assertEqual($expected, $result);
	}

	public function testRefererLocal() {
		$_SERVER['HTTP_REFERER'] = '/posts/index';
		$request = new Request();

		$expected = '/posts/index';
		$result = $request->referer('/', true);
		$this->assertEqual($expected, $result);
	}

	public function testRefererLocalFromNotLocal() {
		$_SERVER['HTTP_REFERER'] = 'http://lithium.com/posts/index';
		$request = new Request();

		$expected = '/';
		$result = $request->referer('/', true);
		$this->assertEqual($expected, $result);
	}

	public function testMagicParamsAccess() {
		$this->assertNull($this->request->action);
		$this->assertFalse(isset($this->request->params['action']));
		$this->assertFalse(isset($this->request->action));

		$expected = $this->request->params['action'] = 'index';
		$this->assertEqual($expected, $this->request->action);
		$this->assertTrue(isset($this->request->action));
	}

	public function testSingleFileNormalization() {
		$_FILES = array(
			'file' => array(
				'name' => 'file.jpg',
				'type' => 'image/jpeg',
				'tmp_name' => '/private/var/tmp/phpows38J',
				'error' => 0,
				'size' => 418
			)
		);
		$request = new Request();

		$expected = array('file' => array(
			'name' => 'file.jpg',
			'type' => 'image/jpeg',
			'tmp_name' => '/private/var/tmp/phpows38J',
			'error' => 0,
			'size' => 418
		));
		$result = $request->data;
		$this->assertEqual($expected, $result);

		unset($_FILES, $request);
	}

	public function testDeepFileNormalization() {
		$_FILES = array(
			'files' => array(
				'name' => array(
					0 => 'file 2.jpg',
					1 => 'file 3.jpg',
					2 => 'file 4.jpg'
				),
				'type' => array(
					0 => 'image/jpeg',
					1 => 'image/jpeg',
					2 => 'image/jpeg'
				),
				'tmp_name' => array(
					0 => '/private/var/tmp/phpF5vsky',
					1 => '/private/var/tmp/phphRJ2zW',
					2 => '/private/var/tmp/phprI92L1'
				),
				'error' => array(
					0 => 0,
					1 => 0,
					2 => 0
				),
				'size' => array(
					0 => 418,
					1 => 418,
					2 => 418
				)
			)
		);
		$request = new Request();

		$expected = array('files' => array(
			0 => array(
				'name' => 'file 2.jpg',
				'type' => 'image/jpeg',
				'tmp_name' => '/private/var/tmp/phpF5vsky',
				'error' => 0,
				'size' => 418
			),
			1 => array(
				'name' => 'file 3.jpg',
				'type' => 'image/jpeg',
				'tmp_name' => '/private/var/tmp/phphRJ2zW',
				'error' => 0,
				'size' => 418
			),
			2 => array(
				'name' => 'file 4.jpg',
				'type' => 'image/jpeg',
				'tmp_name' => '/private/var/tmp/phprI92L1',
				'error' => 0,
				'size' => 418
			)
		));
		$result = $request->data;
		$this->assertEqual($expected, $result);

		unset($_FILES, $request);
	}

	public function testNestedFilesNormalization() {
		$_FILES = array('Image' => array(
			'name' => array(
				'file' => 'file 5.jpg'
			),
			'type' => array(
				'file' => 'image/jpeg'
			),
			'tmp_name' => array(
				'file' => '/private/var/tmp/phpAmSDL4'
			),
			'error' => array(
				'file' => 0
			),
			'size' => array(
				'file' => 418
			)
		));
		$request = new Request();

		$expected = array('Image' => array(
			'file' => array(
				'name' => 'file 5.jpg',
				'type' => 'image/jpeg',
				'tmp_name' => '/private/var/tmp/phpAmSDL4',
				'error' => 0,
				'size' => 418
			)
		));

		$result = $request->data;
		$this->assertEqual($expected, $result);

		unset($_FILES, $request);
	}

	public function testNestedDeepFilesNormalization() {
		$_FILES = array('Photo' => array(
			'name' => array(
				'files' => array(
					0 => 'file 6.jpg',
					1 => 'file 7.jpg',
					2 => 'file 8.jpg'
				)
			),
			'type' => array(
				'files' => array(
					0 => 'image/jpeg',
					1 => 'image/jpeg',
					2 => 'image/jpeg'
				)
			),
			'tmp_name' => array(
				'files' => array(
					0 => '/private/var/tmp/php2eViak',
					1 => '/private/var/tmp/phpMsC5Pp',
					2 => '/private/var/tmp/phpm2nm98'
				)
			),
			'error' => array(
				'files' => array(
					0 => 0,
					1 => 0,
					2 => 0
				)
			),
			'size' => array(
				'files' => array(
					0 => 418,
					1 => 418,
					2 => 418
				)
			)
		));
		$request = new Request();

		$expected = array('Photo' => array(
			'files' => array(
				0 => array(
					'name' => 'file 6.jpg',
					'type' => 'image/jpeg',
					'tmp_name' => '/private/var/tmp/php2eViak',
					'error' => 0,
					'size' => 418
				),
				1 => array(
					'name' => 'file 7.jpg',
					'type' => 'image/jpeg',
					'tmp_name' => '/private/var/tmp/phpMsC5Pp',
					'error' => 0,
					'size' => 418
				),
				2 => array(
					'name' => 'file 8.jpg',
					'type' => 'image/jpeg',
					'tmp_name' => '/private/var/tmp/phpm2nm98',
					'error' => 0,
					'size' => 418
				)
			)
		));
		$result = $request->data;
		$this->assertEqual($expected, $result);

		unset($_FILES, $request);
	}

	public function testMixedFilesNormalization() {
		$_FILES = array(
			'file' => array(
				'name' => 'file.jpg',
				'type' => 'image/jpeg',
				'tmp_name' => '/private/var/tmp/phpows38J',
				'error' => 0,
				'size' => 418
			),
			'files' => array(
				'name' => array(
					0 => 'file 2.jpg',
					1 => 'file 3.jpg',
					2 => 'file 4.jpg'
				),
				'type' => array(
					0 => 'image/jpeg',
					1 => 'image/jpeg',
					2 => 'image/jpeg'
				),
				'tmp_name' => array(
					0 => '/private/var/tmp/phpF5vsky',
					1 => '/private/var/tmp/phphRJ2zW',
					2 => '/private/var/tmp/phprI92L1'
				),
				'error' => array(
					0 => 0,
					1 => 0,
					2 => 0
				),
				'size' => array(
					0 => 418,
					1 => 418,
					2 => 418
				)
			),
			'Image' => array(
				'name' => array(
					'file' => 'file 5.jpg'
				),
				'type' => array(
					'file' => 'image/jpeg'
				),
				'tmp_name' => array(
					'file' => '/private/var/tmp/phpAmSDL4'
				),
				'error' => array(
					'file' => 0
				),
				'size' => array(
					'file' => 418
				)
			),
			'Photo' => array(
				'name' => array(
					'files' => array(
						0 => 'file 6.jpg',
						1 => 'file 7.jpg',
						2 => 'file 8.jpg'
					)
				),
				'type' => array(
					'files' => array(
						0 => 'image/jpeg',
						1 => 'image/jpeg',
						2 => 'image/jpeg'
					)
				),
				'tmp_name' => array(
					'files' => array(
						0 => '/private/var/tmp/php2eViak',
						1 => '/private/var/tmp/phpMsC5Pp',
						2 => '/private/var/tmp/phpm2nm98'
					)
				),
				'error' => array(
					'files' => array(
						0 => 0,
						1 => 0,
						2 => 0
					)
				),
				'size' => array(
					'files' => array(
						0 => 418,
						1 => 418,
						2 => 418
					)
				)
			)
		);
		$expected = array(
			'file' => array(
				'name' => 'file.jpg',
				'type' => 'image/jpeg',
				'tmp_name' => '/private/var/tmp/phpows38J',
				'error' => 0,
				'size' => 418
			),
			'files' => array(
				0 => array(
					'name' => 'file 2.jpg',
					'type' => 'image/jpeg',
					'tmp_name' => '/private/var/tmp/phpF5vsky',
					'error' => 0,
					'size' => 418
				),
				1 => array(
					'name' => 'file 3.jpg',
					'type' => 'image/jpeg',
					'tmp_name' => '/private/var/tmp/phphRJ2zW',
					'error' => 0,
					'size' => 418
				),
				2 => array(
					'name' => 'file 4.jpg',
					'type' => 'image/jpeg',
					'tmp_name' => '/private/var/tmp/phprI92L1',
					'error' => 0,
					'size' => 418
				)
			),
			'Image' => array(
				'file' => array(
					'name' => 'file 5.jpg',
					'type' => 'image/jpeg',
					'tmp_name' => '/private/var/tmp/phpAmSDL4',
					'error' => 0,
					'size' => 418
				)
			),
			'Photo' => array(
				'files' => array(
					0 => array(
						'name' => 'file 6.jpg',
						'type' => 'image/jpeg',
						'tmp_name' => '/private/var/tmp/php2eViak',
						'error' => 0,
						'size' => 418
					),
					1 => array(
						'name' => 'file 7.jpg',
						'type' => 'image/jpeg',
						'tmp_name' => '/private/var/tmp/phpMsC5Pp',
						'error' => 0,
						'size' => 418
					),
					2 => array(
						'name' => 'file 8.jpg',
						'type' => 'image/jpeg',
						'tmp_name' => '/private/var/tmp/phpm2nm98',
						'error' => 0,
						'size' => 418
					)
				)
			)
		);

		$request = new Request();
		$result = $request->data;
		$this->assertEqual($expected, $result);

		unset($_FILES, $request);
	}

	public function testRequestTypeAccessors() {
		$request = new Request(array('env' => array('REQUEST_METHOD' => 'GET')));
		$this->assertTrue($request->is('get'));
		$this->assertFalse($request->is('post'));

		$request = new Request(array('env' => array('REQUEST_METHOD' => 'POST')));
		$this->assertTrue($request->is('post'));
		$this->assertFalse($request->is('get'));
		$this->assertFalse($request->is('put'));

		$request = new Request(array('env' => array('REQUEST_METHOD' => 'PUT')));
		$this->assertTrue($request->is('put'));
		$this->assertFalse($request->is('get'));
		$this->assertFalse($request->is('post'));
	}

	public function testRequestTypeIsMobile() {
		$request = new Request(array('env' => array(
			'HTTP_USER_AGENT' => 'Mozilla/5.0 (iPhone; U; CPU like Mac OS X; en)'
		)));
		$this->assertTrue($request->is('mobile'));
	}

	public function testUrlFromGet() {
		$_GET['url'] = 'posts/1';
		$request = new Request();

		$expected = 'posts/1';
		$result = $request->url;
		$this->assertEqual($expected, $result);

		unset($_GET);
	}

	public function testUrlFromConstructor() {
		$request = new Request(array('url' => 'posts/1'));

		$expected = 'posts/1';
		$result = $request->url;
		$this->assertEqual($expected, $result);
	}

	public function testDataFromConstructor() {
		$request = new Request(array('data' => array('name' => 'bob')));

		$expected = array('name' => 'bob');
		$result = $request->data;
		$this->assertEqual($expected, $result);
	}

	public function testQueryFromConstructor() {
		$request = new Request(array('query' => array('page' => 1)));

		$expected = array('page' => 1);
		$result = $request->query;
		$this->assertEqual($expected, $result);
	}

	public function testMethodOverrideFromData() {
		$_POST['_method'] = 'put';
		$request = new Request();

		$result = $request->is('put');
		$this->assertTrue($result);

		unset($_POST);

		$request = new Request(array('data' => array('_method' => 'put')));

		$result = $request->is('put');
		$this->assertTrue($result);
	}

	public function testMergeMobileDetectors() {
		$request = new Request(array(
			'env' => array('HTTP_USER_AGENT' => 'testMobile'),
			'detectors' => array('mobile' => array('HTTP_USER_AGENT', array('testMobile')))
		));

		$result = $request->is('mobile');
		$this->assertTrue($result);

		$request = new Request(array(
			'env' => array('HTTP_USER_AGENT' => 'iPhone'),
			'detectors' => array('mobile' => array('HTTP_USER_AGENT', array('testMobile')))
		));

		$result = $request->is('mobile');
		$this->assertTrue($result);
	}

	public function testRequestTypeFromConstruct() {
		$request = new Request(array('type' => 'json'));

		$expected = 'json';
		$result = $request->type();
		$this->assertEqual($expected, $result);
	}

	public function testRequestTypeFromParams() {
		$request = new Request();
		$request->params['type'] = 'json';

		$expected = 'json';
		$result = $request->type();
		$this->assertEqual($expected, $result);
	}

	public function testAutomaticContentDecoding() {
		foreach (array('POST', 'PUT', 'PATCH') as $method) {
			$stream = fopen('php://temp', 'r+');
			fwrite($stream, '{ "foo": "bar" }');
			rewind($stream);
			$request = new Request(compact('stream') + array('env' => array(
				'CONTENT_TYPE' => 'application/json; charset=UTF-8',
				'REQUEST_METHOD' => $method
			)));
			$this->assertEqual(array('foo' => 'bar'), $request->data);
		}

		foreach (array('GET', 'HEAD', 'OPTIONS', 'DELETE') as $method) {
			$stream = fopen('php://temp', 'r+');
			fwrite($stream, '{ "foo": "bar" }');
			rewind($stream);
			$request = new Request(compact('stream') + array('env' => array(
				'CONTENT_TYPE' => 'application/json; charset=UTF-8',
				'REQUEST_METHOD' => $method
			)));
			$this->assertFalse($request->data);
		}
	}

	public function testRequestTypeFromHeader() {
		$request = new Request(array('env' => array('CONTENT_TYPE' => 'json')));
		$this->assertEqual('json', $request->type());
	}

	public function testResponseTypeDetection() {
		$request = new Request(array('env' => array('HTTP_ACCEPT' => 'text/xml,*/*')));
		$this->assertEqual('xml', $request->accepts());

		$request->params['type'] = 'json';
		$this->assertEqual('json', $request->accepts());

		$request = new Request(array('env' => array(
			'HTTP_ACCEPT' => 'application/xml,image/png,*/*'
		)));
		$this->assertEqual('xml', $request->accepts());

		$request = new Request(array('env' => array(
			'HTTP_ACCEPT' => 'application/xml,application/xhtml+xml'
		)));
		$this->assertEqual('html', $request->accepts());

		$request = new Request(array('env' => array('HTTP_ACCEPT' => null)));
		$this->assertEqual('html', $request->accepts());
	}

	/**
	 * Tests that accepted content-types without a `q` value are sorted in the order they appear in
	 * the `HTTP_ACCEPT` header.
	 */
	public function testAcceptTypeOrder() {
		$request = new Request(array('env' => array(
			'HTTP_ACCEPT' => 'application/xhtml+xml,text/html'
		)));
		$expected = array('application/xhtml+xml', 'text/html');
		$this->assertEqual($expected, $request->accepts(true));

		$request = new Request(array('env' => array(
			'HTTP_USER_AGENT' => 'Safari',
			'HTTP_ACCEPT' => 'application/xhtml+xml,text/html,text/plain;q=0.9'
		)));
		$expected = array('application/xhtml+xml', 'text/html', 'text/plain');
		$this->assertEqual($expected, $request->accepts(true));
	}

	public function testParsingAcceptHeader() {
		$chrome = array(
			'application/xml',
			'application/xhtml+xml',
			'text/html;q=0.9',
			'text/plain;q=0.8',
			'image/png',
			'*/*;q=0.5'
		);
		$firefox = array(
			'text/html',
			'application/xhtml+xml',
			'application/xml;q=0.9',
			'*/*;q=0.8'
		);
		$safari = array(
			'application/xml',
			'application/xhtml+xml',
			'text/html;q=0.9',
			'text/plain;q=0.8',
			'image/png',
			'*/*;q=0.5'
		);
		$opera = array(
			'text/html',
			'application/xml;q=0.9',
			'application/xhtml+xml',
			'image/png',
			'image/jpeg',
			'image/gif',
			'image/x-xbitmap',
			'*/*;q=0.1'
		);
		$android = array(
			'application/xml',
			'application/xhtml+xml',
			'text/html;q=0.9',
			'text/plain;q=0.8',
			'image/png',
			'*/*;q=0.5',
			'application/youtube-client'
		);
		$request = new Request(array('env' => array('HTTP_ACCEPT' => join(',', $chrome))));
		$this->assertEqual('html', $request->accepts());
		$this->assertTrue(array_search('text/plain', $request->accepts(true)), 4);

		$request = new Request(array('env' => array('HTTP_ACCEPT' => join(',', $safari))));
		$this->assertEqual('html', $request->accepts());

		$request = new Request(array('env' => array('HTTP_ACCEPT' => join(',', $firefox))));
		$this->assertEqual('html', $request->accepts());

		$request = new Request(array('env' => array('HTTP_ACCEPT' => join(',', $opera))));
		$this->assertEqual('html', $request->accepts());

		$request = new Request(array('env' => array('HTTP_ACCEPT' => join(',', $chrome))));
		$request->params['type'] = 'txt';

		$result = $request->accepts(true);
		$this->assertEqual('text/plain', $result[0]);

		$request = new Request(array('env' => array('HTTP_ACCEPT' => join(',', $android))));
		$this->assertEqual('html', $request->accepts());
	}

	/**
	 * Tests that `Accept` headers with only one listed content type are parsed property, and tests
	 * that `'* /*'` is still parsed as `'text/html'`.
	 */
	public function testAcceptSingleContentType() {
		$request = new Request(array('env' => array('HTTP_ACCEPT' => 'application/json,text/xml')));
		$this->assertEqual(array('application/json', 'text/xml'), $request->accepts(true));
		$this->assertEqual('json', $request->accepts());

		$request = new Request(array('env' => array('HTTP_ACCEPT' => 'application/json')));
		$this->assertEqual(array('application/json'), $request->accepts(true));
		$this->assertEqual('json', $request->accepts());

		$request = new Request(array('env' => array('HTTP_ACCEPT' => '*/*')));
		$this->assertEqual(array('text/html'), $request->accepts(true));
		$this->assertEqual('html', $request->accepts());
	}

	public function testLocaleDetection() {
		$request = new Request();
		$this->assertNull($request->locale());

		$request->params['locale'] = 'fr';
		$this->assertEqual('fr', $request->locale());

		$request->locale('de');
		$this->assertEqual('de', $request->locale());
	}

	/**
	 * Tests that `action\Request` correctly inherits the functionality of the `to()` method
	 * inherited from `lithium\net\http\Request`.
	 */
	public function testConvertToUrl() {
		$request = new Request(array(
			'env' => array('HTTP_HOST' => 'foo.com', 'HTTPS' => 'on'),
			'base' => '/the/base/path',
			'url' => '/the/url',
			'query' => array('some' => 'query', 'parameter' => 'values')
		));
		$expected = 'https://foo.com/the/base/path/the/url?some=query&parameter=values';
		$this->assertEqual($expected, $request->to('url'));

		$request = new Request(array(
			'env' => array('HTTP_HOST' => 'foo.com'),
			'base' => '/',
			'url' => '/',
			'query' => array()
		));
		$expected = 'http://foo.com/';
		$this->assertEqual($expected, $request->to('url'));

		$request = new Request(array(
			'url' => 'foo/bar',
			'base' => null,
			'env' => array('HTTP_HOST' => 'example.com', 'PHP_SELF' => '/index.php')
		));

		$expected = 'http://example.com/foo/bar';
		$this->assertEqual($expected, $request->to('url'));
	}

	public function testConvertToUrl2() {
		$request = new Request(array(
			'env' => array('HTTP_HOST' => 'foo.com', 'HTTPS' => 'on'),
			'base' => '/the/base/path',
			'url' => '/posts',
			'params' => array('controller' => 'posts', 'action' => 'index'),
			'query' => array('some' => 'query', 'parameter' => 'values')
		));
		$expected = 'https://foo.com/the/base/path/posts?some=query&parameter=values';
		$this->assertEqual($expected, $request->to('url'));
	}

	/**
	 * Tests that the HTTP request method set by `Request` from the server information is not
	 * overwritten in a parent class.
	 */
	public function testRequesMethodConfiguration() {
		$request = new Request(array('env' => array('REQUEST_METHOD' => 'POST')));
		$this->assertEqual('POST', $request->method);

		$request = new Request(array('env' => array('REQUEST_METHOD' => 'PATCH')));
		$this->assertEqual('PATCH', $request->method);
	}
}

?>