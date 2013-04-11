<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\net\socket;

use lithium\core\NetworkException;

/**
 * A PHP stream-based socket adapter.
 *
 * This stream adapter provides the required method implementations of the abstract `Socket` class
 * for the `open()`, `close()`, `read()`, `write()`, `timeout()` `eof()` and `encoding()` methods.
 *
 * @link http://www.php.net/manual/en/book.stream.php PHP Manual: Streams
 * @see lithium\net\socket\Stream
 */
class Stream extends \lithium\net\Socket {

	/**
	 * Opens a socket and initializes the internal resource handle.
	 *
	 * @param array $options update the config settings
	 * @return mixed Returns `false` if the socket configuration does not contain the
	 *         `'scheme'` or `'host'` settings, or if configuration fails, otherwise returns a
	 *         resource stream. Throws exception if there is a network error.
	 */
	public function open(array $options = array()) {
		parent::open($options);
		$config = $this->_config;

		if (!$config['scheme'] || !$config['host']) {
			return false;
		}
		$scheme = ($config['scheme'] !== 'udp') ? 'tcp' : 'udp';
		$port = $config['port'] ?: 80;
		$host = "{$scheme}://{$config['host']}:{$port}";
		$flags = STREAM_CLIENT_CONNECT;

		if ($config['persistent']) {
			$flags = STREAM_CLIENT_CONNECT | STREAM_CLIENT_PERSISTENT;
		}
		$this->_resource = stream_socket_client(
			$host, $errorCode, $errorMessage, $config['timeout'], $flags
		);
		if ($errorCode || $errorMessage) {
			throw new NetworkException($errorMessage);
		}
		$this->timeout($config['timeout']);

		if (!empty($config['encoding'])) {
			$this->encoding($config['encoding']);
		}
		return $this->_resource;
	}

	/**
	 * Closes the stream
	 *
	 * @return boolean True on closed connection
	 */
	public function close() {
		if (!is_resource($this->_resource)) {
			return true;
		}
		fclose($this->_resource);

		if (is_resource($this->_resource)) {
			$this->close();
		}
		return true;
	}

	/**
	 * Determines if the socket resource is at EOF.
	 *
	 * @return boolean Returns `true` if resource pointer is at its EOF, `false` otherwise.
	 */
	public function eof() {
		return is_resource($this->_resource) ? feof($this->_resource) : true;
	}

	/**
	 * Reads data from the stream resource
	 *
	 * @param integer $length If specified, will read up to $length bytes from the stream.
	 *        If no value is specified, all remaining bytes in the buffer will be read.
	 * @param integer $offset Seek to the specified byte offset before reading.
	 * @return string Returns string read from stream resource on success, false otherwise.
	 */
	public function read($length = null, $offset = null) {
		if (!is_resource($this->_resource)) {
			return false;
		}
		if (!$length) {
			return stream_get_contents($this->_resource);
		}
		return stream_get_contents($this->_resource, $length, $offset);
	}

	/**
	 * writes data to the stream resource
	 *
	 * @param string $data The string to be written.
	 * @return mixed False on error, number of bytes written otherwise.
	 */
	public function write($data = null) {
		if (!is_resource($this->_resource)) {
			return false;
		}
		if (!is_object($data)) {
			$data = $this->_instance($this->_classes['request'], (array) $data + $this->_config);
		}
		return fwrite($this->_resource, (string) $data, strlen((string) $data));
	}

	/**
	 * Set timeout period on a stream.
	 *
	 * @link http://www.php.net/manual/en/function.stream-set-timeout.php
	 *       PHP Manual: stream_set_timeout()
	 * @param integer $time The timeout value in seconds.
	 * @return void
	 */
	public function timeout($time) {
		if (!is_resource($this->_resource)) {
			return false;
		}
		return stream_set_timeout($this->_resource, $time);
	}

	/**
	 * Sets the character set for stream encoding
	 *
	 * Note: This function only exists in PHP 6. For PHP < 6, this method will return void.
	 *
	 * @link http://www.php.net/manual/en/function.stream-encoding.php stream_encoding()
	 * @param string $charset
	 * @return mixed Returns `null` if `stream_encoding()` function does not exist, boolean
	 *         result of `stream_encoding()` otherwise.
	 */
	public function encoding($charset) {
		if (!function_exists('stream_encoding')) {
			return false;
		}
		return is_resource($this->_resource) ? stream_encoding($this->_resource, $charset) : false;
	}
}

?>