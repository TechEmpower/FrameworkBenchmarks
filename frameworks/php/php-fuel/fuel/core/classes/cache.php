<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Fuel\Core;


// Exception thrown when the Cache was found but expired (auto deleted)
class CacheExpiredException extends \CacheNotFoundException {}


class Cache
{

	/**
	 * Loads any default caching settings when available
	 */
	public static function _init()
	{
		\Config::load('cache', true);
	}

	/**
	 * Creates a new cache instance.
	 *
	 * @param   mixed                 The identifier of the cache, can be anything but empty
	 * @param   array|string          Either an array of settings or the storage driver to be used
	 * @return  Cache_Storage_Driver  The new cache object
	 */
	public static function forge($identifier, $config = array())
	{
		// load the default config
		$defaults = \Config::get('cache', array());

		// $config can be either an array of config settings or the name of the storage driver
		if ( ! empty($config) and ! is_array($config) and ! is_null($config))
		{
			$config = array('driver' => $config);
		}

		// Overwrite default values with given config
		$config = array_merge($defaults, (array) $config);

		if (empty($config['driver']))
		{
			throw new \FuelException('No cache driver given or no default cache driver set.');
		}

		$class = '\\Cache_Storage_'.ucfirst($config['driver']);

		// Convert the name to a string when necessary
		$identifier = call_user_func($class.'::stringify_identifier', $identifier);

		// Return instance of the requested cache object
		return new $class($identifier, $config);
	}

	/**
	 * Front for writing the cache, ensures interchangebility of storage drivers. Actual writing
	 * is being done by the _set() method which needs to be extended.
	 *
	 * @param   mixed  The identifier of the cache, can be anything but empty
	 * @param   mixed  The content to be cached
	 * @param   int    The time in seconds until the cache will expire, =< 0 or null means no expiration
	 * @param   array  Contains the identifiers of caches this one will depend on (not supported by all drivers!)
	 * @return  Cache_Storage_Driver  The new Cache object
	 */
	public static function set($identifier, $contents = null, $expiration = false, $dependencies = array())
	{
		$contents = \Fuel::value($contents);

		$cache = static::forge($identifier);
		return $cache->set($contents, $expiration, $dependencies);
	}

	/**
	 * Does get() & set() in one call that takes a callback and it's arguements to generate the contents
	 *
	 * @param   mixed         The identifier of the cache, can be anything but empty
	 * @param   string|array  Valid PHP callback
	 * @param   array         Arguements for the above function/method
	 * @param   int           Cache expiration in seconds
	 * @param   array         Contains the identifiers of caches this one will depend on (not supported by all drivers!)
	 * @return  mixed
	 */
	public static function call($identifier, $callback, $args = array(), $expiration = null, $dependencies = array())
	{
		$cache = static::forge($identifier);
		return $cache->call($callback, $args, $expiration, $dependencies);
	}

	/**
	 * Front for reading the cache, ensures interchangebility of storage drivers. Actual reading
	 * is being done by the _get() method which needs to be extended.
	 *
	 * @param   mixed  The identifier of the cache, can be anything but empty
	 * @param   bool
	 * @return  mixed
	 */
	public static function get($identifier, $use_expiration = true)
	{
		$cache = static::forge($identifier);
		return $cache->get($use_expiration);
	}

	/**
	 * Frontend for deleting item from the cache, interchangable storage methods. Actual operation
	 * handled by delete() call on storage driver class
	 *
	 * @param  mixed  The identifier of the cache, can be anything but empty
	 */
	public static function delete($identifier)
	{
		$cache = static::forge($identifier);
		return $cache->delete();
	}

	/**
	 * Flushes the whole cache for a specific storage driver or just a part of it when $section is set
	 * (might not work with all storage drivers), defaults to the default storage driver
	 *
	 * @param   null|string
	 * @param   null|string
	 * @return  bool
	 */
	public static function delete_all($section = null, $driver = null)
	{
		$cache = static::forge('__NOT_USED__', $driver);
		return $cache->delete_all($section);
	}
}

