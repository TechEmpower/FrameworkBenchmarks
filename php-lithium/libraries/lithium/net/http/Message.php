<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\net\http;

/**
 * Base class for `lithium\net\http\Request` and `lithium\net\http\Response`. Implements basic
 * protocol handling for HTTP-based transactions.
 */
class Message extends \lithium\net\Message {

	/**
	 * The full protocol: HTTP/1.1
	 *
	 * @var string
	 */
	public $protocol = null;

	/**
	 * Specification version number
	 *
	 * @var string
	 */
	public $version = '1.1';

	/**
	 * headers
	 *
	 * @var array
	 */
	public $headers = array();

	/**
	 * Content-Type
	 *
	 * @var string
	 */
	protected $_type = null;

	/**
	 * Classes used by `Request`.
	 *
	 * @var array
	 */
	protected $_classes = array(
		'media' => 'lithium\net\http\Media',
		'auth' => 'lithium\net\http\Auth'
	);

	/**
	 * Adds config values to the public properties when a new object is created.
	 *
	 * @param array $config Configuration options : default value
	 * - `scheme`: http
	 * - `host`: localhost
	 * - `port`: null
	 * - `username`: null
	 * - `password`: null
	 * - `path`: null
	 * - `version`: 1.1
	 * - `headers`: array
	 * - `body`: null
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'scheme' => 'http',
			'host' => 'localhost',
			'port' => null,
			'username' => null,
			'password' => null,
			'path' => null,
			'query' => array(),
			'fragment' => null,
			'protocol' => null,
			'version' => '1.1',
			'headers' => array(),
			'body' => null,
			'auth' => null
		);
		$config += $defaults;
		parent::__construct($config);
		foreach (array_intersect_key(array_filter($config), $defaults) as $key => $value) {
			$this->{$key} = $value;
		}
		if (strpos($this->host, '/') !== false) {
			list($this->host, $this->path) = explode('/', $this->host, 2);
		}
		$this->path = str_replace('//', '/', "/{$this->path}");
		$this->protocol = $this->protocol ?: "HTTP/{$this->version}";
	}

	/**
	 * Add a header to rendered output, or return a single header or full header list.
	 *
	 * @param string $key
	 * @param string $value
	 * @return array
	 */
	public function headers($key = null, $value = null) {
		if (is_string($key) && strpos($key, ':') === false) {
			if ($value === null) {
				return isset($this->headers[$key]) ? $this->headers[$key] : null;
			}
			if ($value === false) {
				unset($this->headers[$key]);
				return $this->headers;
			}
		}
		foreach (($value ? array($key => $value) : (array) $key) as $header => $value) {
			if (!is_string($header)) {
				if (preg_match('/(.*?):(.+)/', $value, $match)) {
					$this->headers[$match[1]] = trim($match[2]);
				}
			} else {
				$this->headers[$header] = $value;
			}
		}
		$headers = array();

		foreach ($this->headers as $key => $value) {
			if (is_array($value)) {
				foreach ($value as $val) {
					$headers[] = "{$key}: {$val}";
				}
				continue;
			}
			$headers[] = "{$key}: {$value}";
		}
		return $headers;
	}

	/**
	 * Sets/gets the content type.
	 *
	 * @param string $type A full content type i.e. `'application/json'` or simple name `'json'`
	 * @return string A simple content type name, i.e. `'html'`, `'xml'`, `'json'`, etc., depending
	 *         on the content type of the request.
	 */
	public function type($type = null) {
		if ($type === false) {
			unset($this->headers['Content-Type']);
			$this->_type = false;
			return;
		}
		$media = $this->_classes['media'];

		if (!$type && $this->_type) {
			return $this->_type;
		}
		$headers = $this->headers + array('Content-Type' => null);
		$type = $type ?: $headers['Content-Type'];

		if (!$type) {
			return;
		}
		$header = $type;

		if (!$data = $media::type($type)) {
			$this->headers('Content-Type', $type);
			return ($this->_type = $type);
		}
		if (is_string($data)) {
			$type = $data;
		} else if (!empty($data['content'])) {
			$header = is_array($data['content']) ? reset($data['content']) : $data['content'];
		}
		$this->headers('Content-Type', $header);
		return ($this->_type = $type);
	}

	/**
	 * Add body parts.
	 *
	 * @param mixed $data
	 * @param array $options
	 *        - `'buffer'`: split the body string
	 * @return array
	 */
	public function body($data = null, $options = array()) {
		$default = array('buffer' => null, 'encode' => false, 'decode' => false);
		$options += $default;
		$body = $this->body = array_filter(array_merge((array) $this->body, (array) $data));

		if (empty($options['buffer']) && empty($body)) {
			return "";
		}
		if ($options['encode']) {
			$body = $this->_encode($body);
		}
		$body = is_array($body) ? join("\r\n", $body) : $body;

		if ($options['decode']) {
			$body = $this->_decode($body);
		}
		return ($options['buffer']) ? str_split($body, $options['buffer']) : $body;
	}

	/**
	 * Encodes the body based on the type
	 *
	 * @see lithium\net\http\Message::type()
	 * @param mixed $body
	 * @return string
	 */
	protected function _encode($body) {
		$media = $this->_classes['media'];

		if ($type = $media::type($this->_type)) {
			$body = $media::encode($this->_type, $body) ?: $body;
		}
		return $body;
	}

	/**
	 * Decodes the body based on the type
	 *
	 * @param string $body
	 * @return mixed
	 */
	protected function _decode($body) {
		$media = $this->_classes['media'];

		if (!$type = $media::type($this->_type)) {
			return $body;
		}
		return $media::decode($this->_type, $body) ?: $body;
	}
}

?>