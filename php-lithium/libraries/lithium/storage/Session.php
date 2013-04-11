<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\storage;

use lithium\core\Libraries;

/**
 * The `Session` static class provides a consistent interface to configure and utilize the
 * different persistent storage adapters included with Lithium, as well as your own adapters.
 *
 * The Session layer of Lithium inherits from the common `Adaptable` class, which provides
 * the generic configuration setting & retrieval logic, as well as the logic required to
 * locate & instantiate the proper adapter class.
 *
 * In most cases, you will configure various named session configurations in your bootstrap
 * process, which will then be available to you in all other parts of your application.
 *
 * Each adapter provides a consistent interface for the basic session operations like `read`,
 * `write`, `delete` and `check`, which can be used interchangeably between all adapters.
 *
 * For more information on `Session` methods and specific adapters, please see their relevant
 * documentation.
 *
 * @see lithium\core\Adaptable
 * @see lithium\storage\session\adapter
 */
class Session extends \lithium\core\Adaptable {

	/**
	 * Stores configurations arrays for session adapters, keyed by configuration name.
	 *
	 * @var array
	 */
	protected static $_configurations = array();

	/**
	 * A dot-separated path for use by `Libraries::locate()`. Used to look up the correct type of
	 * adapters for this class.
	 *
	 * @var string
	 */
	protected static $_adapters = 'adapter.storage.session';

	/**
	 * `Libraries::locate()` compatible path to strategies for this class.
	 *
	 * @var string
	 */
	protected static $_strategies = 'strategy.storage.session';

	/**
	 * Returns the key used to identify the session.
	 *
	 * @param mixed $name Optional named session configuration.
	 * @return string Returns the value of the session identifier key, or `null` if no named
	 *         configuration exists, or no session has been started.
	 */
	public static function key($name = null) {
		return is_object($adapter = static::adapter($name)) ? $adapter->key() : null;
	}

	/**
	 * Indicates whether the the current request includes information on a previously started
	 * session.
	 *
	 * @param string $name Optional named session configuration.
	 * @return boolean Returns `true` if a the request includes a key from a previously created
	 *         session.
	 */
	public static function isStarted($name = null) {
		return is_object($adapter = static::adapter($name)) ? $adapter->isStarted() : false;
	}

	/**
	 * Reads a value from a persistent session store.
	 *
	 * @param string $key Key to be read.
	 * @param array $options Optional parameters that this method accepts:
	 *              - `'name'` _string_: To force the read from a specific adapter, specify the name
	 *                of the configuration (i.e. `'default'`) here.
	 *              - `'strategies'` _boolean_: Indicates whether or not a configuration's applied
	 *                strategy classes should be enabled for this operation. Defaults to `true`.
	 * @return mixed Read result on successful session read, `null` otherwise.
	 * @filter This method may be filtered.
	 */
	public static function read($key = null, array $options = array()) {
		$defaults = array('name' => null, 'strategies' => true);
		$options += $defaults;
		$method = ($name = $options['name']) ? static::adapter($name)->read($key, $options) : null;
		$settings = static::_config($name);

		if (!$method) {
			foreach (array_keys(static::$_configurations) as $name) {
				if ($method = static::adapter($name)->read($key, $options)) {
					break;
				}
			}
			if (!$method || !$name) {
				return null;
			}
		}
		$filters = $settings['filters'] ?: array();
		$result = static::_filter(__FUNCTION__, compact('key', 'options'), $method, $filters);

		if ($options['strategies']) {
			$options += array('key' => $key, 'mode' => 'LIFO', 'class' => __CLASS__);
			return static::applyStrategies(__FUNCTION__, $name, $result, $options);
		}
		return $result;
	}

	/**
	 * Writes a persistent value to one or more session stores.
	 *
	 * @param string $key Key to be written.
	 * @param mixed $value Data to be stored.
	 * @param array $options Optional parameters that this method accepts:
	 *              - `'name'` _string_: To force the write to a specific adapter, specify the name
	 *                of the configuration (i.e. `'default'`) here.
	 *              - `'strategies'` _boolean_: Indicates whether or not a configuration's applied
	 *                strategy classes should be enabled for this operation. Defaults to `true`.
	 * @return boolean Returns `true` on successful write, `false` otherwise.
	 * @filter This method may be filtered.
	 */
	public static function write($key, $value = null, array $options = array()) {
		$defaults = array('name' => null, 'strategies' => true);
		$options += $defaults;

		if (is_resource($value) || !static::$_configurations) {
			return false;
		}
		$methods = array();

		if ($name = $options['name']) {
			$methods = array($name => static::adapter($name)->write($key, $value, $options));
		} else {
			foreach (array_keys(static::$_configurations) as $name) {
				if ($method = static::adapter($name)->write($key, $value, $options)) {
					$methods[$name] = $method;
				}
			}
		}
		$result = false;

		$original = $value;

		foreach ($methods as $name => $method) {
			$settings = static::_config($name);
			$filters = $settings['filters'];
			if ($options['strategies']) {
				$options += array('key' => $key, 'class' => __CLASS__);
				$value = static::applyStrategies(__FUNCTION__, $name, $original, $options);
			}
			$params = compact('key', 'value', 'options');
			$result = static::_filter(__FUNCTION__, $params, $method, $filters) || $result;
		}
		return $result;
	}

	/**
	 * Deletes a named key from a single adapter (if a `'name'` option is specified) or all
	 * session adapters.
	 *
	 * @param string $key The name of the session key to delete.
	 * @param array $options Optional parameters that this method accepts:
	 *              - `'name'` _string_: To force the delete to a specific adapter, specify the name
	 *                of the configuration (i.e. `'default'`) here.
	 *              - `'strategies'` _boolean_: Indicates whether or not a configuration's applied
	 *                strategy classes should be enabled for this operation. Defaults to `true`.
	 * @return boolean Returns `true` on successful delete, or `false` on failure.
	 * @filter This method may be filtered.
	 */
	public static function delete($key, array $options = array()) {
		$defaults = array('name' => null, 'strategies' => true);
		$options += $defaults;

		$methods = array();

		if ($name = $options['name']) {
			$methods = array($name => static::adapter($name)->delete($key, $options));
		} else {
			foreach (static::$_configurations as $name => $config) {
				if ($method = static::adapter($name)->delete($key, $options)) {
					$methods[$name] = $method;
				}
			}
		}
		$result = false;
		$options += array('key' => $key, 'class' => __CLASS__);
		$original = $key;

		foreach ($methods as $name => $method) {
			$settings = static::_config($name);
			if ($options['strategies']) {
				$options += array('key' => $key, 'class' => __CLASS__);
				$key = static::applyStrategies(__FUNCTION__, $name, $original, $options);
			}
			$params = compact('key', 'options');
			$filters = $settings['filters'];
			$result = static::_filter(__FUNCTION__, $params, $method, $filters) || $result;
		}
		return $result;
	}

	/**
	 * Clears all keys from a single adapter (if a `'name'` options is specified) or all
	 * session adapters.
	 *
	 * @param array $options Optional parameters that this method accepts:
	 *              - `'name'` _string_: To force the write to a specific adapter, specify the name
	 *                of the configuration (i.e. `'default'`) here.
	 *              - `'strategies'` _boolean_: Indicates whether or not a configuration's applied
	 *                strategy classes should be enabled for this operation. Defaults to `true`.
	 * @filter
	 */
	public static function clear(array $options = array()) {
		$defaults = array('name' => null, 'strategies' => true);
		$options += $defaults;
		$methods = array();

		if ($name = $options['name']) {
			$methods = array($name => static::adapter($name)->clear($options));
		} else {
			foreach (static::$_configurations as $name => $config) {
				if ($method = static::adapter($name)->clear($options)) {
					$methods[$name] = $method;
				}
			}
		}
		$params = compact('options');
		$result = false;

		foreach ($methods as $name => $method) {
			$settings = static::_config($name);
			$filters = $settings['filters'];
			$result = static::_filter(__FUNCTION__, $params, $method, $filters) || $result;
		}
		if ($options['strategies']) {
			$options += array('mode' => 'LIFO', 'class' => __CLASS__);
			return static::applyStrategies(__FUNCTION__, $name, $result, $options);
		}
		return $result;
	}

	/**
	 * Checks if a session key is set in any adapter, or if a particular adapter configuration is
	 * specified (via `'name'` in `$options`), only that configuration is checked.
	 *
	 * @param string $key The session key to check.
	 * @param array $options Optional parameters that this method accepts.
	 * @return boolean
	 * @filter This method may be filtered.
	 */
	public static function check($key, array $options = array()) {
		$defaults = array('name' => null, 'strategies' => true);
		$options += $defaults;
		$methods = array();

		if ($name = $options['name']) {
			$methods = array($name => static::adapter($name)->check($key, $options));
		} else {
			foreach (static::$_configurations as $name => $config) {
				if ($method = static::adapter($name)->check($key, $options)) {
					$methods[$name] = $method;
				}
			}
		}
		$params = compact('key', 'options');
		$result = false;

		foreach ($methods as $name => $method) {
			$settings = static::_config($name);
			$filters = $settings['filters'];
			$result = static::_filter(__FUNCTION__, $params, $method, $filters) || $result;
		}
		if ($options['strategies']) {
			$options += array('key' => $key, 'mode' => 'LIFO', 'class' => __CLASS__);
			return static::applyStrategies(__FUNCTION__, $name, $result, $options);
		}
		return $result;
	}

	/**
	 * Returns the adapter object instance of the named configuration.
	 *
	 * @param string $name Named configuration. If not set, the last configured
	 *        adapter object instance will be returned.
	 * @return object Adapter instance.
	 */
	public static function adapter($name = null) {
		if (!$name) {
			if (!$names = array_keys(static::$_configurations)) {
				return;
			}
			$name = end($names);
		}
		return parent::adapter($name);
	}
}

?>