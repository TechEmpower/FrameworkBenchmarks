<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\net\http;

use lithium\core\Libraries;
use lithium\core\ClassNotFoundException;

/**
 * Basic Http Service.
 *
 */
class Service extends \lithium\core\Object {

	/**
	 * The `Socket` instance used to send `Service` calls.
	 *
	 * @var lithium\net\Socket
	 */
	public $connection = null;

	/**
	 * Holds the last request and response object
	 *
	 * @var object
	 */
	public $last = null;

	/**
	 * Auto config
	 *
	 * @var array
	 */
	protected $_autoConfig = array('classes' => 'merge', 'responseTypes');

	/**
	 * Array of closures that return various pieces of information about an HTTP response.
	 *
	 * @var array
	 */
	protected $_responseTypes = array();

	/**
	 * Indicates whether `Service` can connect to the HTTP endpoint for which it is configured.
	 * Defaults to true until a connection attempt fails.
	 *
	 * @var boolean
	 */
	protected $_isConnected = false;

	/**
	 * Fully-name-spaced class references to `Service` class dependencies.
	 *
	 * @var array
	 */
	protected $_classes = array(
		'media'    => 'lithium\net\http\Media',
		'request'  => 'lithium\net\http\Request',
		'response' => 'lithium\net\http\Response'
	);

	/**
	 * Initializes a new `Service` instance with the default HTTP request settings and
	 * transport- and format-handling classes.
	 * @param array $config
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'persistent' => false,
			'scheme'     => 'http',
			'host'       => 'localhost',
			'port'       => null,
			'timeout'    => 30,
			'auth'       => null,
			'username'   => null,
			'password'   => null,
			'encoding'   => 'UTF-8',
			'socket'     => 'Context'
		);
		parent::__construct($config + $defaults);
	}

	/**
	 * Initialize connection
	 *
	 */
	protected function _init() {
		$config = array('classes' => $this->_classes) + $this->_config;

		try {
			$this->connection = Libraries::instance('socket', $config['socket'], $config);
		} catch(ClassNotFoundException $e) {
			$this->connection = null;
		}
		$this->_responseTypes += array(
			'headers' => function($response) { return $response->headers; },
			'body' => function($response) { return $response->body(); },
			'code' => function($response) { return $response->status['code']; }
		);
	}

	/**
	 * Magic method to handle other HTTP methods.
	 *
	 * @param string $method
	 * @param string $params
	 */
	public function __call($method, $params = array()) {
		array_unshift($params, $method);
		return $this->invokeMethod('send', $params);
	}

	/**
	 * Custom check to determine if our given magic methods can be responded to.
	 *
	 * @param  string  $method     Method name.
	 * @param  bool    $internal   Interal call or not.
	 * @return bool
	 */
	public function respondsTo($method, $internal = false) {
		return is_callable(array($this, $method), true);
	}

	/**
	 * Send HEAD request.
	 *
	 * @param string $path
	 * @param array $data
	 * @param array $options
	 * @return string
	 */
	public function head($path = null, $data = array(), array $options = array()) {
		$defaults = array('return' => 'headers', 'type' => false);
		return $this->send(__FUNCTION__, $path, $data, $options + $defaults);
	}

	/**
	 * Send GET request.
	 *
	 * @param string $path
	 * @param array $data
	 * @param array $options
	 * @return string
	 */
	public function get($path = null, $data = array(), array $options = array()) {
		$defaults = array('type' => false);
		return $this->send(__FUNCTION__, $path, $data, $options + $defaults);
	}

	/**
	 * Send POST request.
	 *
	 * @param string $path
	 * @param array $data
	 * @param array $options
	 * @return string
	 */
	public function post($path = null, $data = array(), array $options = array()) {
		return $this->send(__FUNCTION__, $path, $data, $options);
	}

	/**
	 * Send PUT request.
	 *
	 * @param string $path
	 * @param array $data
	 * @param array $options
	 * @return string
	 */
	public function put($path = null, $data = array(), array $options = array()) {
		return $this->send(__FUNCTION__, $path, $data, $options);
	}

	/**
	 * Send PATCH request.
	 *
	 * @param string $path
	 * @param array $data
	 * @param array $options
	 * @return string
	 */
	public function patch($path = null, $data = array(), array $options = array()) {
		return $this->send(__FUNCTION__, $path, $data, $options);
	}

	/**
	 * Send DELETE request.
	 *
	 * @param string $path
	 * @param array $data
	 * @param array $options
	 * @return string
	 */
	public function delete($path = null, $data = array(), array $options = array()) {
		$defaults = array('type' => false);
		return $this->send(__FUNCTION__, $path, $data, $options + $defaults);
	}

	/**
	 * Send request and return response data.
	 *
	 * @param string $method
	 * @param string $path
	 * @param array $data the parameters for the request. For GET/DELETE this is the query string
	 *        for POST/PUT this is the body
	 * @param array $options passed to request and socket
	 * @return string
	 */
	public function send($method, $path = null, $data = array(), array $options = array()) {
		$defaults = array('return' => 'body');
		$options += $defaults;
		$request = $this->_request($method, $path, $data, $options);
		$options += array('message' => $request);

		if (!$this->connection || !$this->connection->open($options)) {
			return;
		}
		$response = $this->connection->send($request, $options);
		$this->connection->close();

		if ($response->status['code'] == 401 && $auth = $response->digest()) {
			$request->auth = $auth;
			$this->connection->open(array('message' => $request) + $options);
			$response = $this->connection->send($request, $options);
			$this->connection->close();
		}
		$this->last = (object) compact('request', 'response');

		$handlers = $this->_responseTypes;
		$handler = isset($handlers[$options['return']]) ? $handlers[$options['return']] : null;

		return $handler ? $handler($response) : $response;
	}

	/**
	 * Instantiates a request object (usually an instance of `http\Request`) and tests its
	 * properties based on the request type and data to be sent.
	 *
	 * @param string $method The HTTP method of the request, i.e. `'GET'`, `'HEAD'`, `'OPTIONS'`,
	 *        etc. Can be passed in upper- or lower-case.
	 * @param string $path The
	 * @param string $data
	 * @param string $options
	 * @return object Returns an instance of `http\Request`, configured with an HTTP method, query
	 *         string or POST/PUT/PATCH data, and URL.
	 */
	protected function _request($method, $path, $data, $options) {
		$defaults = array('type' => 'form');
		$options += $defaults + $this->_config;

		$request = $this->_instance('request', $options);
		$request->path = str_replace('//', '/', "{$request->path}{$path}");
		$request->method = $method = strtoupper($method);
		$hasBody = in_array($method, array('POST', 'PUT', 'PATCH'));
		$hasBody ? $request->body($data) : $request->query = $data;
		return $request;
	}
}

?>