<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\storage\cache\adapter;

use Redis as RedisCore;

/**
 * A Redis (phpredis) cache adapter implementation.
 *
 * This adapter uses the `phpredis` PHP extension, which can be found here:
 * https://github.com/nicolasff/phpredis
 *
 * The Redis cache adapter is meant to be used through the `Cache` interface,
 * which abstracts away key generation, adapter instantiation and filter
 * implementation. This adapter does not aim to provide a full implementation of the
 * Redis API, but rather only a subset of its features that are useful in the context of a
 * semi-persistent cache.
 *
 * A simple configuration of this adapter can be accomplished in `config/bootstrap/cache.php`
 * as follows:
 *
 * {{{
 * Cache::config(array(
 *     'cache-config-name' => array(
 *         'adapter' => 'Redis',
 *         'host' => '127.0.0.1:6379'
 *     )
 * ));
 * }}}
 *
 * The 'host' key accepts a string argument in the format of ip:port where the Redis
 * server can be found.
 *
 * This Redis adapter provides basic support for `write`, `read`, `delete`
 * and `clear` cache functionality, as well as allowing the first four
 * methods to be filtered as per the Lithium filtering system.
 *
 * @see lithium\storage\Cache::key()
 * @see lithium\storage\Cache::adapter()
 * @link https://github.com/nicolasff/phpredis GitHub: PhpRedis Extension
 *
 */
class Redis extends \lithium\core\Object {

	/**
	 * Redis object instance used by this adapter.
	 *
	 * @var object Redis object
	 */
	public $connection;

	/**
	 * Object constructor
	 *
	 * Instantiates the `Redis` object and connects it to the configured server.
	 *
	 * @todo Implement configurable & optional authentication
	 * @see lithium\storage\Cache::config()
	 * @see lithium\storage\cache\adapter\Redis::_ttl()
	 * @param array $config Configuration parameters for this cache adapter.
	 *        These settings are indexed by name and queryable through `Cache::config('name')`. The
	 *        available settings for this adapter are as follows:
	 *        - `'host'` _string_: A string in the form of `'host:port'` indicating the Redis server
	 *          to connect to. Defaults to `'127.0.0.1:6379'`.
	 *        - `'expiry'` _mixed_: Default expiration for cache values written through this
	 *          adapter. Defaults to `'+1 hour'`. For acceptable values, see the `$expiry` parameter
	 *          of `Redis::_ttl()`.
	 *        - `'persistent'` _boolean_: Indicates whether the adapter should use a persistent
	 *          connection when attempting to connect to the Redis server. If `true`, it will
	 *          attempt to reuse an existing connection when connecting, and the connection will
	 *          not close when the request is terminated. Defaults to `false`.
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'host' => '127.0.0.1:6379',
			'expiry' => '+1 hour',
			'persistent' => false
		);
		parent::__construct($config + $defaults);
	}

	/**
	 * Initialize the Redis connection object and connect to the Redis server.
	 *
	 * @return void
	 */
	protected function _init() {
		if (!$this->connection) {
			$this->connection = new RedisCore();
		}
		list($ip, $port) = explode(':', $this->_config['host']);
		$method = $this->_config['persistent'] ? 'pconnect' : 'connect';
		$this->connection->{$method}($ip, $port);
	}

	/**
	 * Dispatches a not-found method to the Redis connection object.
	 *
	 * That way, one can easily use a custom method on that redis adapter like that:
	 *
	 * {{{Cache::adapter('named-of-redis-config')->methodName($argument);}}}
	 *
	 * If you want to know, what methods are available, have a look at the readme of phprdis.
	 * One use-case might be to query possible keys, e.g.
	 *
	 * {{{Cache::adapter('redis')->keys('*');}}}
	 *
	 * @link https://github.com/nicolasff/phpredis GitHub: PhpRedis Extension
	 * @param string $method Name of the method to call
	 * @param array $params Parameter list to use when calling $method
	 * @return mixed Returns the result of the method call
	 */
	public function __call($method, $params = array()) {
		return call_user_func_array(array(&$this->connection, $method), $params);
	}

	/**
	 * Custom check to determine if our given magic methods can be responded to.
	 *
	 * @param  string  $method     Method name.
	 * @param  bool    $internal   Interal call or not.
	 * @return bool
	 */
	public function respondsTo($method, $internal = 0) {
		$parentRespondsTo = parent::respondsTo($method, $internal);
		return $parentRespondsTo || is_callable(array($this->connection, $method));
	}

	/**
	 * Sets expiration time for cache keys
	 *
	 * @param string $key The key to uniquely identify the cached item
	 * @param mixed $expiry A `strtotime()`-compatible string indicating when the cached item
	 *              should expire, or a Unix timestamp.
	 * @return boolean Returns `true` if expiry could be set for the given key, `false` otherwise.
	 */
	protected function _ttl($key, $expiry) {
		return $this->connection->expireAt($key, is_int($expiry) ? $expiry : strtotime($expiry));
	}

	/**
	 * Write value(s) to the cache
	 *
	 * @param string $key The key to uniquely identify the cached item
	 * @param mixed $value The value to be cached
	 * @param null|string $expiry A strtotime() compatible cache time. If no expiry time is set,
	 *        then the default cache expiration time set with the cache configuration will be used.
	 * @return closure Function returning boolean `true` on successful write, `false` otherwise.
	 */
	public function write($key, $value = null, $expiry = null) {
		$connection =& $this->connection;
		$expiry = ($expiry) ?: $this->_config['expiry'];
		$_self =& $this;

		return function($self, $params) use (&$_self, &$connection, $expiry) {
			if (is_array($params['key'])) {
				$expiry = $params['data'];

				if ($connection->mset($params['key'])) {
					$ttl = array();

					if ($expiry) {
						foreach ($params['key'] as $k => $v) {
							$ttl[$k] = $_self->invokeMethod('_ttl', array($k, $expiry));
						}
					}
					return $ttl;
				}
			}
			if ($result = $connection->set($params['key'], $params['data'])) {
				if ($expiry) {
					return $_self->invokeMethod('_ttl', array($params['key'], $expiry));
				}
				return $result;
			}
		};
	}

	/**
	 * Read value(s) from the cache
	 *
	 * @param string $key The key to uniquely identify the cached item
	 * @return closure Function returning cached value if successful, `false` otherwise
	 */
	public function read($key) {
		$connection =& $this->connection;

		return function($self, $params) use (&$connection) {
			$key = $params['key'];

			if (is_array($key)) {
				return $connection->getMultiple($key);
			}
			return $connection->get($key);
		};
	}

	/**
	 * Delete value from the cache
	 *
	 * @param string $key The key to uniquely identify the cached item
	 * @return closure Function returning boolean `true` on successful delete, `false` otherwise
	 */
	public function delete($key) {
		$connection =& $this->connection;

		return function($self, $params) use (&$connection) {
			return (boolean) $connection->delete($params['key']);
		};
	}

	/**
	 * Performs an atomic decrement operation on specified numeric cache item.
	 *
	 * Note that if the value of the specified key is *not* an integer, the decrement
	 * operation will have no effect whatsoever. Redis chooses to not typecast values
	 * to integers when performing an atomic decrement operation.
	 *
	 * @param string $key Key of numeric cache item to decrement
	 * @param integer $offset Offset to decrement - defaults to 1.
	 * @return closure Function returning item's new value on successful decrement, else `false`
	 */
	public function decrement($key, $offset = 1) {
		$connection =& $this->connection;

		return function($self, $params) use (&$connection, $offset) {
			return $connection->decr($params['key'], $offset);
		};
	}

	/**
	 * Performs an atomic increment operation on specified numeric cache item.
	 *
	 * Note that if the value of the specified key is *not* an integer, the increment
	 * operation will have no effect whatsoever. Redis chooses to not typecast values
	 * to integers when performing an atomic increment operation.
	 *
	 * @param string $key Key of numeric cache item to increment
	 * @param integer $offset Offset to increment - defaults to 1.
	 * @return closure Function returning item's new value on successful increment, else `false`
	 */
	public function increment($key, $offset = 1) {
		$connection =& $this->connection;

		return function($self, $params) use (&$connection, $offset) {
			return $connection->incr($params['key'], $offset);
		};
	}

	/**
	 * Clears user-space cache
	 *
	 * @return mixed True on successful clear, false otherwise
	 */
	public function clear() {
		return $this->connection->flushdb();
	}

	/**
	 * Determines if the Redis extension has been installed and
	 * that there is a redis-server available
	 *
	 * @return boolean Returns `true` if the Redis extension is enabled, `false` otherwise.
	 */
	public static function enabled() {
		return extension_loaded('redis');
	}
}

?>