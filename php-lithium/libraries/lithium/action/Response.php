<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2012, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\action;

/**
 * A `Response` object is typically instantiated automatically by the `Controller`. It is assigned
 * any headers set in the course of the request, as well as any content rendered by the
 * `Controller`. Once completed, the `Controller` returns the `Response` object to the `Dispatcher`.
 *
 * The `Response` object is responsible for writing its body content to output, and writing any
 * headers to the browser.
 *
 * @see lithium\action\Dispatcher
 * @see lithium\action\Controller
 */
class Response extends \lithium\net\http\Response {

	/**
	 * Classes used by Response.
	 *
	 * @var array
	 */
	protected $_classes = array(
		'router' => 'lithium\net\http\Router',
		'media' => 'lithium\net\http\Media'
	);

	protected $_autoConfig = array('classes' => 'merge');

	public function __construct(array $config = array()) {
		$defaults = array(
			'buffer' => 8192,
			'location' => null,
			'status' => 0,
			'request' => null,
			'decode' => false
		);
		parent::__construct($config + $defaults);
	}

	protected function _init() {
		parent::_init();
		$config = $this->_config;
		$this->status($config['status']);
		unset($this->_config['status']);

		if ($config['location']) {
			$classes = $this->_classes;
			$location = $classes['router']::match($config['location'], $config['request']);
			$this->headers('Location', $location);
		}
	}

	/**
	 * Controls how or whether the client browser and web proxies should cache this response.
	 *
	 * @param mixed $expires This can be a Unix timestamp indicating when the page expires, or a
	 *              string indicating the relative time offset that a page should expire, i.e.
	 *              `"+5 hours". Finally, `$expires` can be set to `false` to completely disable
	 *              browser or proxy caching.
	 * @return void
	 */
	public function cache($expires) {
		if ($expires === false) {
			return $this->headers(array(
				'Expires' => 'Mon, 26 Jul 1997 05:00:00 GMT',
				'Cache-Control' => array(
					'no-store, no-cache, must-revalidate',
					'post-check=0, pre-check=0',
					'max-age=0'
				),
				'Pragma' => 'no-cache'
			));
		}
		$expires = is_int($expires) ? $expires : strtotime($expires);

		return $this->headers(array(
			'Expires' => gmdate('D, d M Y H:i:s', $expires) . ' GMT',
			'Cache-Control' => 'max-age=' . ($expires - time()),
			'Pragma' => 'cache'
		));
	}

	/**
	 * Sets/Gets the content type. If `'type'` is null, the method will attempt to determine the
	 * type from the params, then from the environment setting
	 *
	 * @param string $type a full content type i.e. `'application/json'` or simple name `'json'`
	 * @return string A simple content type name, i.e. `'html'`, `'xml'`, `'json'`, etc., depending
	 *         on the content type of the request.
	 */
	public function type($type = null) {
		if ($type === null && $this->_type === null) {
			$type = 'html';
		}
		return parent::type($type);
	}

	/**
	 * Render a response by writing headers and output. Output is echoed in chunks because of an
	 * issue where `echo` time increases exponentially on long message bodies.
	 *
	 * @return void
	 */
	public function render() {
		$code = null;
		$hasLocation = (isset($this->headers['location']) || isset($this->headers['Location']));

		if ($hasLocation && $this->status['code'] === 200) {
			$code = 302;
		}
		$this->_writeHeader($this->status($code) ?: $this->status(500));

		foreach ($this->headers as $name => $value) {
			$key = strtolower($name);

			if ($key == 'location') {
				$this->_writeHeader("Location: {$value}", $this->status['code']);
			} elseif ($key == 'download') {
				$this->_writeHeader('Content-Disposition: attachment; filename="' . $value . '"');
			} elseif (is_array($value)) {
				$this->_writeHeader(
					array_map(function($v) use ($name) { return "{$name}: {$v}"; }, $value)
				);
			} elseif (!is_numeric($name)) {
				$this->_writeHeader("{$name}: {$value}");
			}
		}
		if ($code == 302 || $code == 204) {
			return;
		}
		$chunked = $this->body(null, $this->_config);

		foreach ($chunked as $chunk) {
			echo $chunk;
		}
	}

	/**
	 * Casts the Response object to a string.  This doesn't actually return a string, but does
	 * a direct render and returns null.
	 *
	 * @return string An empty string.
	 */
	public function __toString() {
		$this->render();
		return '';
	}

	/**
	 * Writes raw headers to output.
	 *
	 * @param string|array $header Either a raw header string, or an array of header strings. Use
	 *        an array if a single header must be written multiple times with different values.
	 *        Otherwise, additional values for duplicate headers will overwrite previous values.
	 * @param integer $code Optional. If present, forces a specific HTTP response code.  Used
	 *        primarily in conjunction with the 'Location' header.
	 * @return void
	 */
	protected function _writeHeader($header, $code = null) {
		if (is_array($header)) {
			array_map(function($h) { header($h, false); }, $header);
			return;
		}
		$code ? header($header, true, $code) : header($header, true);
	}
}

?>