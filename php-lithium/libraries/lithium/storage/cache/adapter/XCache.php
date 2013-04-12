<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\storage\cache\adapter;

/**
 * An XCache opcode cache adapter implementation.
 *
 * The XCache adapter is meant to be used through the `Cache` interface,
 * which abstracts away key generation, adapter instantiation and filter
 * implementation.
 *
 * A simple configuration of this adapter can be accomplished in `config/bootstrap/cache.php`
 * as follows:
 *
 * {{{
 * use lithium\storage\Cache;
 *
 * Cache::config(array(
 *     'cache-config-name' => array(
 *         'adapter' => 'XCache',
 *         'username' => 'user',
 *         'password' => 'pass'
 *     )
 * ));
 * }}}
 *
 * Note that the `username` and `password` configuration fields are only required if
 * you wish to use `XCache::clear()` - all other methods do not require XCache administrator
 * credentials.
 *
 * This XCache adapter provides basic support for `write`, `read`, `delete`
 * and `clear` cache functionality, as well as allowing the first four
 * methods to be filtered as per the Lithium filtering system.
 *
 * This adapter does *not* allow multi-key operations for any methods.
 *
 * @see lithium\storage\Cache::key()
 * @see lithium\storage\cache\adapter
 */
class XCache extends \lithium\core\Object {

	/**
	 * Class constructor.
	 *
	 * @param array $config
	 */
	public function __construct(array $config = array()) {
		$defaults = array('prefix' => '', 'expiry' => '+1 hour');
		parent::__construct($config + $defaults);
	}

	/**
	 * Write value(s) to the cache
	 *
	 * @param string $key The key to uniquely identify the cached item
	 * @param mixed $data The value to be cached
	 * @param null|string $expiry A strtotime() compatible cache time. If no expiry time is set,
	 *        then the default cache expiration time set with the cache configuration will be used.
	 * @return closure Function returning boolean `true` on successful write, `false` otherwise.
	 */
	public function write($key, $data, $expiry = null) {
		$expiry = ($expiry) ?: $this->_config['expiry'];

		return function($self, $params) use ($expiry) {
			return xcache_set($params['key'], $params['data'], strtotime($expiry) - time());
		};
	}

	/**
	 * Read value(s) from the cache
	 *
	 * @param string $key The key to uniquely identify the cached item
	 * @return closure Function returning cached value if successful, `false` otherwise
	 */
	public function read($key) {
		return function($self, $params) {
			return xcache_get($params['key']);
		};
	}

	/**
	 * Delete value from the cache
	 *
	 * @param string $key The key to uniquely identify the cached item
	 * @return closure Function returning boolean `true` on successful delete, `false` otherwise
	 */
	public function delete($key) {
		return function($self, $params) {
			return xcache_unset($params['key']);
		};
	}

	/**
	 * Performs an atomic decrement operation on specified numeric cache item.
	 *
	 * Note that, as per the XCache specification:
	 * If the item's value is not numeric, it is treated as if the value were 0.
	 * It is possible to decrement a value into the negative integers.
	 *
	 * @param string $key Key of numeric cache item to decrement
	 * @param integer $offset Offset to decrement - defaults to 1.
	 * @return closure Function returning item's new value on successful decrement, else `false`
	 */
	public function decrement($key, $offset = 1) {
		return function($self, $params) use ($offset) {
			return xcache_dec($params['key'], $offset);
		};
	}

	/**
	 * Performs an atomic increment operation on specified numeric cache item.
	 *
	 * Note that, as per the XCache specification:
	 * If the item's value is not numeric, it is treated as if the value were 0.
	 *
	 * @param string $key Key of numeric cache item to increment
	 * @param integer $offset Offset to increment - defaults to 1.
	 * @return closure Function returning item's new value on successful increment, else `false`
	 */
	public function increment($key, $offset = 1) {
		return function($self, $params) use ($offset) {
			extract($params);
			return xcache_inc($params['key'], $offset);
		};
	}


	/**
	 * Clears user-space cache.
	 *
	 * This method requires valid XCache admin credentials to be set when the
	 * adapter was configured, due to the use of the xcache_clear_cache admin method.
	 *
	 * If the xcache.admin.enable_auth ini setting is set to "Off", no credentials
	 * required.
	 *
	 * @return mixed True on successful clear, false otherwise.
	 */
	public function clear() {
		$admin = (ini_get('xcache.admin.enable_auth') === "On");
		if ($admin && (!isset($this->_config['username']) || !isset($this->_config['password']))) {
			return false;
		}
		$credentials = array();

		if (isset($_SERVER['PHP_AUTH_USER'])) {
			$credentials['username'] = $_SERVER['PHP_AUTH_USER'];
			$_SERVER['PHP_AUTH_USER'] = $this->_config['username'];
		}
		if (isset($_SERVER['PHP_AUTH_PW'])) {
			$credentials['password'] = $_SERVER['PHP_AUTH_PW'];
			$_SERVER['PHP_AUTH_PW'] = $this->_config['pass'];
		}

		for ($i = 0, $max = xcache_count(XC_TYPE_VAR); $i < $max; $i++) {
			if (xcache_clear_cache(XC_TYPE_VAR, $i) === false) {
				return false;
			}
		}

		if (isset($_SERVER['PHP_AUTH_USER'])) {
			$_SERVER['PHP_AUTH_USER'] = $credentials['username'];
		}
		if (isset($_SERVER['PHP_AUTH_PW'])) {
			$_SERVER['PHP_AUTH_PW'] = $credentials['password'];
		}
		return true;
	}

	/**
	 * Determines if the XCache extension has been installed and
	 * if the userspace cache is available.
	 *
	 * return boolean True if enabled, false otherwise
	 * @return boolean
	 */
	public static function enabled() {
		return extension_loaded('xcache');
	}
}

?>