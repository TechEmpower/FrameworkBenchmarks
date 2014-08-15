<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\storage\session\adapter;

use RuntimeException;
use lithium\util\Set;

/**
 * A minimal adapter to interface with HTTP cookies.
 *
 * This adapter provides basic support for `write`, `read` and `delete`
 * cookie handling, as well as allowing these three methods to be filtered as
 * per the Lithium filtering system.
 *
 */
class Cookie extends \lithium\core\Object {

	/**
	 * Default settings for this session adapter.
	 *
	 * @var array Keys are in direct correspondence with the parameters in the PHP-native
	 *      `setcookie()` method. The only difference is that the `expire` value is a
	 *		strtotime-compatible string instead of an epochal timestamp.
	 */
	protected $_defaults = array(
		'expire' => '+2 days', 'path' => '/',
		'domain' => '', 'secure' => false, 'httponly' => false
	);

	/**
	 * Class constructor.
	 *
	 * Takes care of setting appropriate configurations for this object.
	 *
	 * @param array $config Optional configuration parameters.
	 */
	public function __construct(array $config = array()) {
		if (empty($config['name'])) {
			$config['name'] = basename(LITHIUM_APP_PATH) . 'cookie';
		}
		parent::__construct($config + $this->_defaults);
	}

	/**
	 * Obtain the top-level cookie key.
	 *
	 * @return string The configured cookie 'name' parameter
	 */
	public function key() {
		return $this->_config['name'];
	}

	/**
	 * Determines if cookies are enabled.
	 *
	 * @return boolean True
	 * @todo Implement
	 */
	public function isEnabled() {
		return true;
	}

	/**
	 * Obtain the status of the cookie storage.
	 *
	 * @return boolean True if $_COOKIE has been initialized, false otherwise.
	 */
	public function isStarted() {
		return (isset($_COOKIE));
	}

	/**
	 * Checks if a value has been set in the cookie.
	 *
	 * @param string $key Key of the entry to be checked.
	 * @return closure Function returning boolean `true` if the key exists, `false` otherwise.
	 */
	public function check($key) {
		$config = $this->_config;

		return function($self, $params) use (&$config) {
			return (isset($_COOKIE[$config['name']][$params['key']]));
		};
	}

	/**
	 * Read a value from the cookie.
	 *
	 * @param null|string $key Key of the entry to be read. If $key is null, returns
	 *        all cookie key/value pairs that have been set.
	 * @param array $options Options array. Not used in this adapter.
	 * @return closure Function returning data in the session if successful, `null` otherwise.
	 */
	public function read($key = null, array $options = array()) {
		$config = $this->_config;

		return function($self, $params) use (&$config) {
			$key = $params['key'];
			if (!$key) {
				if (isset($_COOKIE[$config['name']])) {
					return $_COOKIE[$config['name']];
				}
				return array();
			}
			if (strpos($key, '.') !== false) {
				$key = explode('.', $key);
				$result = (isset($_COOKIE[$config['name']])) ? $_COOKIE[$config['name']] : array();

				foreach ($key as $k) {
					if (!isset($result[$k])) {
						return null;
					}
					$result = $result[$k];
				}
				return $result;
			}
			if (isset($_COOKIE[$config['name']][$key])) {
				return $_COOKIE[$config['name']][$key];
			}
		};
	}

	/**
	 * Write a value to the cookie store.
	 *
	 * @param string $key Key of the item to be stored.
	 * @param mixed $value The value to be stored.
	 * @param array $options Options array.
	 * @return closure Function returning boolean `true` on successful write, `false` otherwise.
	 */
	public function write($key, $value = null, array $options = array()) {
		$expire = (!isset($options['expire']) && empty($this->_config['expire']));
		$config = $this->_config;
		$cookieClass = __CLASS__;

		if ($expire && $key !== $config['name']) {
			return null;
		}
		$expires = (isset($options['expire'])) ? $options['expire'] : $config['expire'];

		return function($self, $params) use (&$config, &$expires, $cookieClass) {
			$key = $params['key'];
			$value = $params['value'];
			$key = array($key => $value);
			if (is_array($value)) {
				$key = Set::flatten($key);
			}

			foreach ($key as $name => $val) {
				$name = $cookieClass::keyFormat($name, $config);
				$result = setcookie($name, $val, strtotime($expires), $config['path'],
					$config['domain'], $config['secure'], $config['httponly']
				);

				if (!$result) {
					throw new RuntimeException("There was an error setting {$name} cookie.");
				}
			}
			return true;
		};
	}

	/**
	 * Delete a value from the cookie store.
	 *
	 * @param string $key The key to be deleted from the cookie store.
	 * @param array $options Options array.
	 * @return closure Function returning boolean `true` on successful delete, `false` otherwise.
	 */
	public function delete($key, array $options = array()) {
		$config = $this->_config;
		$cookieClass = get_called_class();

		return function($self, $params) use (&$config, $cookieClass) {
			$key = $params['key'];
			$path = '/' . str_replace('.', '/', $config['name'] . '.' . $key) . '/.';
			$cookies = current(Set::extract($_COOKIE, $path));
			if (is_array($cookies)) {
				$cookies = array_keys(Set::flatten($cookies));
				foreach ($cookies as &$name) {
					$name = $key . '.' . $name;
				}
			} else {
				$cookies = array($key);
			}
			foreach ($cookies as &$name) {
				$name = $cookieClass::keyFormat($name, $config);
				$result = setcookie($name, "", 1, $config['path'],
					$config['domain'], $config['secure'], $config['httponly']
				);
				if (!$result) {
					throw new RuntimeException("There was an error deleting {$name} cookie.");
				}
			}
			return true;
		};
	}

	/**
	 * Clears all cookies.
	 *
	 * @param array $options Options array. Not used fro this adapter method.
	 * @return boolean True on successful clear, false otherwise.
	 */
	public function clear(array $options = array()) {
		$options += array('destroySession' => true);
		$config = $this->_config;
		$cookieClass = get_called_class();

		return function($self, $params) use (&$config, $options, $cookieClass) {
			if ($options['destroySession'] && session_id()) {
				session_destroy();
			}
			if (!isset($_COOKIE[$config['name']])) {
				return true;
			}
			$cookies = array_keys(Set::flatten($_COOKIE[$config['name']]));
			foreach ($cookies as $name) {
				$name = $cookieClass::keyFormat($name, $config);
				$result = setcookie($name, "", 1, $config['path'],
					$config['domain'], $config['secure'], $config['httponly']
				);
				if (!$result) {
					throw new RuntimeException("There was an error clearing {$cookie} cookie.");
				}
			}
			unset($_COOKIE[$config['name']]);
			return true;
		};
	}

	/**
	 * Formats the given `$name` argument for use in the cookie adapter.
	 *
	 * @param string $name The key to be formatted, e.g. `foo.bar.baz`.
	 * @param array $config
	 * @return string The formatted key.
	 */
	public static function keyFormat($name, $config) {
		return $config['name'] . '[' . str_replace('.', '][', $name) . ']';
	}
}

?>