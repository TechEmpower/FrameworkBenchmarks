<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\net\socket;

/**
 * A socket adapter that uses PHP stream contexts.
 */
class Context extends \lithium\net\Socket {

	/**
	 * Connection timeout value.
	 *
	 * @var integer
	 */
	protected $_timeout = 30;

	/**
	 * Content of the stream
	 *
	 * @var string
	 */
	protected $_content = null;

	/**
	 * Constructor
	 *
	 * @param array $config
	 */
	public function __construct(array $config = array()) {
		$defaults = array('mode' => 'r', 'message' => null);
		parent::__construct($config + $defaults);
		$this->timeout($this->_config['timeout']);
	}

	/**
	 * Opens the socket and sets its timeout value.
	 *
	 * @param array $options Update the config settings.
	 * @return mixed Returns `false` if the socket configuration does not contain the
	 *         `'scheme'` or `'host'` settings, or if configuration fails, otherwise returns a
	 *         resource stream.
	 */
	public function open(array $options = array()) {
		parent::open($options);
		$config = $this->_config;

		if (!$config['scheme'] || !$config['host']) {
			return false;
		}
		$url = "{$config['scheme']}://{$config['host']}:{$config['port']}";
		$context = array($config['scheme'] => array('timeout' => $this->_timeout));

		if (is_object($config['message'])) {
			$url = $config['message']->to('url');
			$context = $config['message']->to('context', array('timeout' => $this->_timeout));
		}
		$this->_resource = fopen($url, $config['mode'], false, stream_context_create($context));
		return $this->_resource;
	}

	/**
	 * Closes the socket connection.
	 *
	 * @return boolean Success.
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
	 * End of file test for this socket connection. Does not apply to this implementation.
	 *
	 * @return boolean Success.
	 */
	public function eof() {
		if (!is_resource($this->_resource)) {
			return true;
		}
		return feof($this->_resource);
	}

	/**
	 * Reads from the socket. Does not apply to this implementation.
	 *
	 * @return void
	 */
	public function read() {
		if (!is_resource($this->_resource)) {
			return false;
		}
		$meta = stream_get_meta_data($this->_resource);
		if (isset($meta['wrapper_data'])) {
			$headers = join("\r\n", $meta['wrapper_data']) . "\r\n\r\n";
		} else {
			$headers = null;
		}
		return $headers . stream_get_contents($this->_resource);
	}

	/**
	 * Writes to the socket.
	 *
	 * @param string $data Data to write.
	 * @return boolean Success
	 */
	public function write($data = null) {
		if (!is_resource($this->_resource)) {
			return false;
		}
		if (!is_object($data)) {
			$data = $this->_instance($this->_classes['request'], (array) $data + $this->_config);
		}
		return stream_context_set_option(
			$this->_resource, $data->to('context', array('timeout' => $this->_timeout))
		);
	}

	/**
	 * Sets the timeout on the socket *connection*.
	 *
	 * @param integer $time Seconds after the connection times out.
	 * @return booelan `true` if timeout has been set, `false` otherwise.
	 */
	public function timeout($time = null) {
		if ($time !== null) {
			$this->_timeout = $time;
		}
		return $this->_timeout;
	}

	/**
	 * Sets the encoding of the socket connection. Does not apply to this implementation.
	 *
	 * @param string $charset The character set to use.
	 * @return boolean `true` if encoding has been set, `false` otherwise.
	 */
	public function encoding($charset = null) {
		return false;
	}
}

?>