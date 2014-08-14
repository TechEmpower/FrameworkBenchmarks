<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\action;

class MockNginxRequest extends \lithium\action\Request {

	protected function _init() {
		$safari  = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7) AppleWebKit/534.48.3 ';
		$safari .= '(KHTML, like Gecko) Version/5.1 Safari/534.48.3';
		parent::_init();

		$this->_env = array(
			'FCGI_ROLE' => 'RESPONDER',
			'PATH_INFO' => '',
			'PATH_TRANSLATED' => '/lithium/app/webroot/index.php',
			'QUERY_STRING' => '',
			'REQUEST_METHOD' => 'GET',
			'CONTENT_TYPE' => '',
			'CONTENT_LENGTH' => '',
			'SCRIPT_NAME' => '/index.php',
			'SCRIPT_FILENAME' => '/lithium/app/webroot/index.php',
			'REQUEST_URI' => '/',
			'DOCUMENT_URI' => '/index.php',
			'DOCUMENT_ROOT' => '/lithium/app/webroot',
			'SERVER_PROTOCOL' => 'HTTP/1.1',
			'GATEWAY_INTERFACE' => 'CGI/1.1',
			'REMOTE_ADDR' => '127.0.0.1',
			'REMOTE_PORT' => '52987',
			'SERVER_ADDR' => '127.0.0.1',
			'SERVER_PORT' => '80',
			'SERVER_NAME' => 'sandbox.local',
			'HTTP_HOST' => 'sandbox.local',
			'HTTP_USER_AGENT' => $safari,
			'HTTP_ACCEPT' => 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
			'HTTP_ACCEPT_LANGUAGE' => 'en-us',
			'HTTP_ACCEPT_ENCODING' => 'gzip, deflate',
			'HTTP_CONNECTION' => 'keep-alive',
			'PHP_SELF' => '/index.php'
		);
	}
}

?>