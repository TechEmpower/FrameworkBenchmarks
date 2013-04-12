<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\storage\cache\adapter;

/**
 * An Alternative PHP Cache (APC) cache adapter implementation.
 *
 * The APC cache adapter is meant to be used through the `Cache` interface,
 * which abstracts away key generation, adapter instantiation and filter
 * implementation.
 *
 * A simple configuration of this adapter can be accomplished in `config/bootstrap/cache.php`
 * as follows:
 *
 * {{{
 * Cache::config(array(
 *     'cache-config-name' => array('adapter' => 'Apc')
 * ));
 * }}}
 *
 * This APC adapter provides basic support for `write`, `read`, `delete`
 * and `clear` cache functionality, as well as allowing the first four
 * methods to be filtered as per the Lithium filtering system. Additionally,
 * This adapter defines several methods that are _not_ implemented in other
 * adapters, and are thus non-portable - see the documentation for `Cache`
 * as to how these methods should be accessed.
 *
 * This adapter supports multi-key `write`, `read` and `delete` operations.
 *
 * Learn more about APC in the [PHP APC manual](http://php.net/manual/en/book.apc.php).
 *
 * @see lithium\storage\Cache::key()
 */
class Apc extends \lithium\core\Object {

	/**
	 * Class constructor.
	 *
	 * @param array $config
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'prefix' => '',
			'expiry' => '+1 hour'
		);
		parent::__construct($config + $defaults);
	}

	/**
	 * Write value(s) to the cache.
	 *
	 * This adapter method supports multi-key write. By specifying `$key` as an
	 * associative array of key/value pairs, `$data` is ignored and all keys that
	 * are cached will receive an expiration time of `$expiry`.
	 *
	 * @param string|array $key The key to uniquely identify the cached item.
	 * @param mixed $data The value to be cached.
	 * @param null|string $expiry A strtotime() compatible cache time. If no expiry time is set,
	 *        then the default cache expiration time set with the cache configuration will be used.
	 * @return closure Function returning boolean `true` on successful write, `false` otherwise.
	 */
	public function write($key, $data, $expiry = null) {
		$expiry = ($expiry) ?: $this->_config['expiry'];

		return function($self, $params) use ($expiry) {
			$cachetime = (is_int($expiry) ? $expiry : strtotime($expiry)) - time();
			$key = $params['key'];

			if (is_array($key)) {
				return apc_store($key, $cachetime);
			}
			return apc_store($params['key'], $params['data'], $cachetime);
		};
	}

	/**
	 * Read value(s) from the cache.
	 *
	 * This adapter method supports multi-key reads. By specifying `$key` as an
	 * array of key names, this adapter will attempt to return an array of data
	 * containing key/value pairs of the requested data.
	 *
	 * @param string|array $key The key to uniquely identify the cached item.
	 * @return closure Function returning cached value on successful read, `false` otherwise.
	 */
	public function read($key) {
		return function($self, $params) {
			return apc_fetch($params['key']);
		};
	}

	/**
	 * Delete value from the cache.
	 *
	 * This adapter method supports multi-key deletes. By specifynig `$key` as an
	 * array of key names, this adapter method will attempt to remove these keys
	 * from the user space cache.
	 *
	 * @param string|array $key The key to uniquely identify the cached item.
	 * @return closure Function returning `true` on successful delete, `false` otherwise.
	 */
	public function delete($key) {
		return function($self, $params) {
			return apc_delete($params['key']);
		};
	}

	/**
	 * Performs an atomic decrement operation on specified numeric cache item.
	 *
	 * Note that, as per the APC specification:
	 * If the item's value is not numeric, the decrement operation has no effect
	 * on the key - it retains it's original non-integer value.
	 *
	 * @param string $key Key of numeric cache item to decrement
	 * @param integer $offset Offset to decrement - defaults to 1.
	 * @return closure Function returning item's new value on successful decrement, else `false`
	 */
	public function decrement($key, $offset = 1) {
		return function($self, $params) use ($offset) {
			return apc_dec($params['key'], $offset);
		};
	}

	/**
	 * Performs an atomic increment operation on specified numeric cache item.
	 *
	 * Note that, as per the APC specification:
	 * If the item's value is not numeric, the increment operation has no effect
	 * on the key - it retains it's original non-integer value.
	 *
	 * @param string $key Key of numeric cache item to increment
	 * @param integer $offset Offset to increment - defaults to 1.
	 * @return closure Function returning item's new value on successful increment, else `false`
	 */
	public function increment($key, $offset = 1) {
		return function($self, $params) use ($offset) {
			return apc_inc($params['key'], $offset);
		};
	}

	/**
	 * Clears user-space cache
	 *
	 * @return mixed True on successful clear, false otherwise
	 */
	public function clear() {
		return apc_clear_cache('user');
	}

	/**
	 * Determines if the APC extension has been installed and
	 * if the userspace cache is available.
	 *
	 * @return boolean `true` if enabled, `false` otherwise
	 */
	public static function enabled() {
		$loaded = extension_loaded('apc');
		$isCli = (php_sapi_name() === 'cli');
		$enabled = (!$isCli && ini_get('apc.enabled')) || ($isCli && ini_get('apc.enable_cli'));
		return ($loaded && $enabled);
	}
}

?>