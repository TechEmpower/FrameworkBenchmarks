<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\storage\session\strategy;

use lithium\core\ConfigException;

/**
 * This strategy allows you to encrypt your `Session` and / or `Cookie` data so that it
 * is not stored in cleartext on the client side. You must provide a secret key, otherwise
 * an exception is raised.
 *
 * To use this class, you need to have the `mcrypt` extension enabled.
 *
 * Example configuration:
 *
 * {{{
 * Session::config(array('default' => array(
 *	'adapter' => 'Cookie',
 *	'strategies' => array('Encrypt' => array('secret' => 'f00bar$l1thium'))
 * )));
 * }}}
 *
 * By default, this strategy uses the AES algorithm in the CBC mode. This means that an
 * initialization vector has to be generated and transported with the payload data. This
 * is done transparently, but you may want to keep this in mind (the ECB mode doesn't require
 * an initialization vector but is not recommended to use as it's insecure). You can override this
 * defaults by passing a different `cipher` and/or `mode` to the config like this:
 *
 * {{{
 * Session::config(array('default' => array(
 *	'adapter' => 'Cookie',
 *	'strategies' => array('Encrypt' => array(
 *		'cipher' => MCRYPT_RIJNDAEL_256,
 *		'mode' 	 => MCRYPT_MODE_ECB, // Don't use ECB when you don't have to!
 *		'secret'	 => 'f00bar$l1thium'
 *	))
 * )));
 * }}}
 *
 * Please keep in mind that it is generally not a good idea to store sensitive information in
 * cookies (or generally on the client side) and this class is no exception to the rule. It allows
 * you to store client side data in a more secure way, but 100% security can't be achieved.
 *
 * Also note that if you provide a secret that is shorter than the maximum key length of the
 * algorithm used, the secret will be hashed to make it more secure. This also means that if you
 * want to use your own hashing algorithm, make sure it has the maximum key length of the algorithm
 * used. See the `Encrypt::_hashSecret()` method for more information on this.
 *
 * @link http://php.net/manual/en/book.mcrypt.php The mcrypt extension.
 * @link http://www.php.net/manual/en/mcrypt.ciphers.php List of supported ciphers.
 * @link http://www.php.net/manual/en/mcrypt.constants.php List of supported modes.
 */
class Encrypt extends \lithium\core\Object {

	/**
	 * Holds the initialization vector.
	 */
	protected static $_vector = null;

	/**
	 * Holds the crypto resource after initialization.
	 */
	protected static $_resource = null;

	/**
	 * Default configuration.
	 */
	protected $_defaults = array(
		'cipher' => MCRYPT_RIJNDAEL_128,
		'mode' => MCRYPT_MODE_CBC
	);

	/**
	 * Constructor.
	 *
	 * @param array $config Configuration array. You can override the default cipher and mode.
	 */
	public function __construct(array $config = array()) {
		if (!static::enabled()) {
			throw new ConfigException("The Mcrypt extension is not installed or enabled.");
		}
		if (!isset($config['secret'])) {
			throw new ConfigException("Encrypt strategy requires a secret key.");
		}
		parent::__construct($config + $this->_defaults);

		$cipher = $this->_config['cipher'];
		$mode = $this->_config['mode'];

		static::$_resource = mcrypt_module_open($cipher, '', $mode, '');
		$this->_config['vector'] = static::_vector();
	}

	/**
	 * Destructor.
	 *
	 * Closes the crypto resource when it is no longer needed.
	 */
	public function __destruct() {
		mcrypt_module_close(static::$_resource);
	}

	/**
	 * Read encryption method.
	 *
	 * @param array $data the Data being read.
	 * @param array $options Options for this method.
	 * @return mixed Returns the decrypted key or the dataset.
	 */
	public function read($data, array $options = array()) {
		$class = $options['class'];

		$encrypted = $class::read(null, array('strategies' => false));
		$key = isset($options['key']) ? $options['key'] : null;

		if (!isset($encrypted['__encrypted']) || !$encrypted['__encrypted']) {
			return isset($encrypted[$key]) ? $encrypted[$key] : null;
		}

		$current = $this->_decrypt($encrypted['__encrypted']);

		if ($key) {
			return isset($current[$key]) ? $current[$key] : null;
		} else {
			return $current;
		}
	}

	/**
	 * Write encryption method.
	 *
	 * @param mixed $data The data to be encrypted.
	 * @param array $options Options for this method.
	 * @return string Returns the written data in cleartext.
	 */
	public function write($data, array $options = array()) {
		$class = $options['class'];

		$futureData = $this->read(null, array('key' => null) + $options) ?: array();
		$futureData = array($options['key'] => $data) + $futureData;

		$payload = empty($futureData) ? null : $this->_encrypt($futureData);

		$class::write('__encrypted', $payload, array('strategies' => false) + $options);
		return $payload;
	}

	/**
	 * Delete encryption method.
	 *
	 * @param mixed $data The data to be encrypted.
	 * @param array $options Options for this method.
	 * @return string Returns the deleted data in cleartext.
	 */
	public function delete($data, array $options = array()) {
		$class = $options['class'];

		$futureData = $this->read(null, array('key' => null) + $options) ?: array();
		unset($futureData[$options['key']]);

		$payload = empty($futureData) ? null : $this->_encrypt($futureData);

		$class::write('__encrypted', $payload, array('strategies' => false) + $options);
		return $data;
	}

	/**
	 * Serialize and encrypt a given data array.
	 *
	 * @param array $decrypted The cleartext data to be encrypted.
	 * @return string A Base64 encoded and encrypted string.
	 */
	protected function _encrypt($decrypted = array()) {
		$vector = $this->_config['vector'];
		$secret = $this->_hashSecret($this->_config['secret']);

		mcrypt_generic_init(static::$_resource, $secret, $vector);
		$encrypted = mcrypt_generic(static::$_resource, serialize($decrypted));
		mcrypt_generic_deinit(static::$_resource);

		return base64_encode($encrypted) . base64_encode($vector);
	}

	/**
	 * Decrypt and unserialize a previously encrypted string.
	 *
	 * @param string $encrypted The base64 encoded and encrypted string.
	 * @return array The cleartext data.
	 */
	protected function _decrypt($encrypted) {
		$secret = $this->_hashSecret($this->_config['secret']);

		$vectorSize = strlen(base64_encode(str_repeat(" ", static::_vectorSize())));
		$vector = base64_decode(substr($encrypted, -$vectorSize));
		$data = base64_decode(substr($encrypted, 0, -$vectorSize));

		mcrypt_generic_init(static::$_resource, $secret, $vector);
		$decrypted = mdecrypt_generic(static::$_resource, $data);
		mcrypt_generic_deinit(static::$_resource);

		return unserialize(trim($decrypted));
	}

	/**
	 * Determines if the Mcrypt extension has been installed.
	 *
	 * @return boolean `true` if enabled, `false` otherwise.
	 */
	public static function enabled() {
		return extension_loaded('mcrypt');
	}

	/**
	 * Hashes the given secret to make harder to detect.
	 *
	 * This method figures out the appropriate key size for the chosen encryption algorithm and
	 * then hashes the given key accordingly. Note that if the key has already the needed length,
	 * it is considered to be hashed (secure) already and is therefore not hashed again. This lets
	 * you change the hashing method in your own code if you like.
	 *
	 * The default `MCRYPT_RIJNDAEL_128` key should be 32 byte long `sha256` is used as the hashing
	 * algorithm. If the key size is shorter than the one generated by `sha256`, the first n bytes
	 * will be used.
	 *
	 * @link http://www.php.net/manual/de/function.mcrypt-enc-get-key-size.php
	 * @param string $key The possibly too weak key.
	 * @return string The hashed (raw) key.
	 */
	protected function _hashSecret($key) {
		$size = mcrypt_enc_get_key_size(static::$_resource);

		if (strlen($key) >= $size) {
			return $key;
		}

		return substr(hash('sha256', $key, true), 0, $size);
	}

	/**
	 * Generates an initialization vector.
	 *
	 * @return string Returns an initialization vector.
	 * @link http://www.php.net/manual/en/function.mcrypt-create-iv.php
	 */
	protected static function _vector() {
		if (static::$_vector) {
			return static::$_vector;
		}

		return static::$_vector = mcrypt_create_iv(static::_vectorSize(), MCRYPT_DEV_URANDOM);
	}

	/**
	 * Returns the vector size vor a given cipher and mode.
	 *
	 * @return number The vector size.
	 * @link http://www.php.net/manual/en/function.mcrypt-enc-get-iv-size.php
	 */
	protected static function _vectorSize() {
		return mcrypt_enc_get_iv_size(static::$_resource);
	}
}

?>