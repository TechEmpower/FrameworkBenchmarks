<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\storage;

/**
 * The `Cache` static class provides a consistent interface to configure and utilize the different
 * cache adapters included with Lithium, as well as your own adapters.
 *
 * The Cache layer of Lithium inherits from the common `Adaptable` class, which provides the generic
 * configuration setting & retrieval logic, as well as the logic required to locate & instantiate
 * the proper adapter class.
 *
 * In most cases, you will configure various named cache configurations in your bootstrap process,
 * which will then be available to you in all other parts of your application.
 *
 * A simple example configuration:
 *
 * {{{Cache::config(array(
 *     'local' => array('adapter' => 'Apc'),
 *     'distributed' => array(
 *         'adapter' => 'Memcached',
 *         'host' => '127.0.0.1:11211',
 *     ),
 *     'default' => array('adapter' => 'File')
 * ));}}}
 *
 * Each adapter provides a consistent interface for the basic cache operations of `write`, `read`,
 * `delete` and `clear`, which can be used interchangeably between all adapters. Some adapters
 * may provide additional methods that are not consistently available across other adapters.
 * To make use of these, it is always possible to call:
 *
 * {{{Cache::adapter('named-configuration')->methodName($argument);}}}
 *
 * This allows a very wide range of flexibility, at the cost of portability.
 *
 * Some cache adapters (e.g. `File`) do _not_ provide the functionality for increment/decrement.
 * Additionally, some cache adapters support multi-key operations for `write`, `read` and `delete`
 * &mdash; please see the individual documentation for cache adapters and the operations that
 * they support.
 *
 * @see lithium\core\Adaptable
 * @see lithium\storage\cache\adapter
 */
class Cache extends \lithium\core\Adaptable {

	/**
	 * Stores configurations for cache adapters
	 *
	 * @var array
	 */
	protected static $_configurations = array();

	/**
	 * Libraries::locate() compatible path to adapters for this class.
	 *
	 * @var string Dot-delimited path.
	 */
	protected static $_adapters = 'adapter.storage.cache';

	/**
	 * Libraries::locate() compatible path to strategies for this class.
	 *
	 * @var string Dot-delimited path.
	 */
	protected static $_strategies = 'strategy.storage.cache';

	/**
	 * Generates the cache key.
	 *
	 * @param mixed $key A string (or lambda/closure that evaluates to a string)
	 *                    that will be used as the cache key.
	 * @param array $data If a lambda/closure is used as a key and requires arguments,
	 *                    pass them in here.
	 * @return string The generated cache key.
	 */
	public static function key($key, $data = array()) {
		return is_object($key) ? $key($data) : $key;
	}

	/**
	 * Writes to the specified cache configuration.
	 *
	 * @param string $name Configuration to be used for writing
	 * @param mixed $key Key to uniquely identify the cache entry
	 * @param mixed $data Data to be cached
	 * @param string $expiry A strtotime() compatible cache time
	 * @param mixed $options Options for the method, filters and strategies.
	 * @return boolean True on successful cache write, false otherwise
	 * @filter This method may be filtered.
	 */
	public static function write($name, $key, $data, $expiry = null, array $options = array()) {
		$options += array('conditions' => null, 'strategies' => true);
		$settings = static::config();

		if (!isset($settings[$name])) {
			return false;
		}
		$conditions = $options['conditions'];

		if (is_callable($conditions) && !$conditions()) {
			return false;
		}
		$key = static::key($key, $data);

		if (is_array($key)) {
			$expiry = $data;
			$data = null;
		}

		if ($options['strategies']) {
			$options = array('key' => $key, 'class' => __CLASS__);
			$data = static::applyStrategies(__FUNCTION__, $name, $data, $options);
		}

		$method = static::adapter($name)->write($key, $data, $expiry);
		$params = compact('key', 'data', 'expiry');
		return static::_filter(__FUNCTION__, $params, $method, $settings[$name]['filters']);
	}


	/**
	 * Reads from the specified cache configuration
	 *
	 * @param string $name Configuration to be used for reading
	 * @param mixed $key Key to be retrieved
	 * @param mixed $options Options for the method and strategies.
	 * @return mixed Read results on successful cache read, null otherwise
	 * @filter This method may be filtered.
	 */
	public static function read($name, $key, array $options = array()) {
		$options += array('conditions' => null, 'strategies' => true, 'write' => null);
		$settings = static::config();

		if (!isset($settings[$name])) {
			return false;
		}
		$conditions = $options['conditions'];

		if (is_callable($conditions) && !$conditions()) {
			return false;
		}
		$key = static::key($key);
		$method = static::adapter($name)->read($key);
		$params = compact('key');
		$filters = $settings[$name]['filters'];
		$result = static::_filter(__FUNCTION__, $params, $method, $filters);

		if ($result === null && ($write = $options['write'])) {
			$write = is_callable($write) ? $write() : $write;
			list($expiry, $value) = each($write);
			$value = is_callable($value) ? $value() : $value;

			if (static::write($name, $key, $value, $expiry)) {
				$result = $value;
			}
		}

		if ($options['strategies']) {
			$options = compact('key') + array('mode' => 'LIFO', 'class' => __CLASS__);
			$result = static::applyStrategies(__FUNCTION__, $name, $result, $options);
		}
		return $result;
	}

	/**
	 * Delete a value from the specified cache configuration
	 *
	 * @param string $name The cache configuration to delete from
	 * @param mixed $key Key to be deleted
	 * @param mixed $options Options for the method and strategies.
	 * @return boolean True on successful deletion, false otherwise
	 * @filter This method may be filtered.
	 */
	public static function delete($name, $key, array $options = array()) {
		$options += array('conditions' => null, 'strategies' => true);
		$settings = static::config();

		if (!isset($settings[$name])) {
			return false;
		}
		$conditions = $options['conditions'];

		if (is_callable($conditions) && !$conditions()) {
			return false;
		}

		$key = static::key($key);
		$method = static::adapter($name)->delete($key);
		$filters = $settings[$name]['filters'];

		if ($options['strategies']) {
			$options += array('key' => $key, 'class' => __CLASS__);
			$key = static::applyStrategies(__FUNCTION__, $name, $key, $options);
		}
		return static::_filter(__FUNCTION__, compact('key'), $method, $filters);
	}

	/**
	 * Performs an atomic increment operation on specified numeric cache item
	 * from the given cache configuration.
	 *
	 * @param string $name
	 * @param string $key Key of numeric cache item to increment
	 * @param integer $offset Offset to increment - defaults to 1.
	 * @param mixed $options Options for this method.
	 * @return mixed Item's new value on successful increment, false otherwise.
	 * @filter This method may be filtered.
	 */
	public static function increment($name, $key, $offset = 1, array $options = array()) {
		$options += array('conditions' => null);
		$settings = static::config();

		if (!isset($settings[$name])) {
			return false;
		}
		$conditions = $options['conditions'];

		if (is_callable($conditions) && !$conditions()) {
			return false;
		}

		$key = static::key($key);
		$method = static::adapter($name)->increment($key, $offset);
		$params = compact('key', 'offset');
		$filters = $settings[$name]['filters'];

		return static::_filter(__FUNCTION__, $params, $method, $filters);
	}

	/**
	 * Performs an atomic decrement operation on specified numeric cache item
	 * from the given cache configuration.
	 *
	 * @param string $name
	 * @param string $key Key of numeric cache item to decrement
	 * @param integer $offset Offset to decrement - defaults to 1.
	 * @param mixed $options Options for this method.
	 * @return mixed Item's new value on successful decrement, false otherwise.
	 * @filter This method may be filtered.
	 */
	public static function decrement($name, $key, $offset = 1, array $options = array()) {
		$options += array('conditions' => null);
		$settings = static::config();

		if (!isset($settings[$name])) {
			return false;
		}
		$conditions = $options['conditions'];

		if (is_callable($conditions) && !$conditions()) {
			return false;
		}

		$key = static::key($key);
		$method = static::adapter($name)->decrement($key, $offset);
		$params = compact('key', 'offset');
		$filters = $settings[$name]['filters'];

		return static::_filter(__FUNCTION__, $params, $method, $filters);
	}

	/**
	 * Perform garbage collection on specified cache configuration.
	 *
	 * This method is not filterable.
	 *
	 * @param string $name The cache configuration to be cleaned
	 * @return boolean True on successful clean, false otherwise
	 */
	public static function clean($name) {
		$settings = static::config();
		return (isset($settings[$name])) ? static::adapter($name)->clean() : false;
	}

	/**
	 * Remove all cache keys from specified configuration.
	 *
	 * This method is non-filterable.
	 *
	 * @param string $name The cache configuration to be cleared
	 * @return boolean True on successful clearing, false otherwise
	 */
	public static function clear($name) {
		$settings = static::config();
		return (isset($settings[$name])) ? static::adapter($name)->clear() : false;
	}
}

?>