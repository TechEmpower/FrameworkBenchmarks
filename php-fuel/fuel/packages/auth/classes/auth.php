<?php
/**
 * Fuel is a fast, lightweight, community driven PHP5 framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Auth;


class AuthException extends \FuelException {}


/**
 * Auth
 *
 * @package     Fuel
 * @subpackage  Auth
 */
class Auth
{

	/**
	 * @var  Auth_Login_Driver	default instance
	 */
	protected static $_instance = null;

	/**
	 * @var  Array  contains references if multiple were loaded
	 */
	protected static $_instances = array();

	/**
	 * @var  Array  Login drivers that verified a current login
	 */
	protected static $_verified = array();

	/**
	 * @var  bool  Whether to verify multiple
	 */
	protected static $_verify_multiple = false;

	/**
	 * @var  Array  subdriver registry, takes driver name and method for checking it
	 */
	protected static $_drivers = array(
		'group'  => 'member',
		'acl'    => 'has_access',
	);

	public static function _init()
	{
		\Config::load('auth', true);

		// Whether to allow multiple drivers of any type, defaults to not allowed
		static::$_verify_multiple = \Config::get('auth.verify_multiple_logins', false);

		foreach((array) \Config::get('auth.driver', array()) as $driver => $config)
		{
			$config = is_int($driver)
				? array('driver' => $config)
				: array_merge($config, array('driver' => $driver));
			static::forge($config);
		}
		// set the first (or only) as the default instance for static usage
		if ( ! empty(static::$_instances))
		{
			static::$_instance = reset(static::$_instances);
			static::check();
		}
	}

	/**
	 * Load a login driver to the array of loaded drivers
	 *
	 * @param   Array  settings for the new driver
	 * @throws  AuthException  on driver load failure
	 */
	public static function forge($custom = array())
	{
		// Driver is given as array key or just string in custom
		$custom = ! is_array($custom) ? array('driver' => $custom) : $custom;
		$config = \Config::get('auth.'.$custom['driver'].'_config', array());
		$config = array_merge($config, $custom);

		// Driver must be set
		if (empty($config['driver']) || ! is_string($config['driver']))
		{
			throw new \AuthException('No auth driver given.');
		}

		// determine the driver to load
		$driver = \Auth_Login_Driver::forge($config);

		// get the driver's cookie name
		$id = $driver->get_id();

		// do we already have a driver instance for this cookie?
		if (isset(static::$_instances[$id]))
		{
			// if so, they must be using the same driver class!
			$class = get_class($driver);
			if ( ! static::$_instances[$id] instanceof $class)
			{
				throw new \AuthException('You can not instantiate two different login drivers using the same id "'.$id.'"');
			}
		}
		else
		{
			// store this instance
			static::$_instances[$id] = $driver;
		}

		return static::$_instances[$id];
	}

	/**
	 * Prevent instantiation
	 */
	final private function __construct() {}

	/**
	 * Remove individual driver, or all drivers of $type
	 *
	 * @param   string  driver id or null for default driver
	 * @throws  AuthException  when $driver_id isn't valid or true
	 */
	public static function unload($driver_id = null)
	{
		if ($driver_id === null && ! empty(static::$_instance))
		{
			unset(static::$_instances[static::$_instance->get_id()]);
			static::$_instance = null;
			return true;
		}
		elseif (array_key_exists($driver_id, static::$_instances))
		{
			return false;
		}

		unset(static::$_instances[$driver_id]);
		return true;
	}

	/**
	 * Return a specific driver, or the default instance (is created if necessary)
	 *
	 * @param   string  driver id
	 * @return  Auth_Login_Driver
	 */
	public static function instance($instance = null)
	{
		if ($instance !== null)
		{
			if ( ! array_key_exists($instance, static::$_instances))
			{
				return false;
			}

			return static::$_instances[$instance];
		}

		if (static::$_instance === null)
		{
			static::$_instance = static::forge();
		}

		return static::$_instance;
	}

	/**
	 * Check login drivers for validated login
	 *
	 * @param   string|Array  specific driver or drivers, in this case it will always terminate after first success
	 * @return  bool
	 */
	public static function check($specific = null)
	{
		$drivers = $specific === null ? static::$_instances : (array) $specific;

		foreach ($drivers as $i)
		{
			if ( ! static::$_verify_multiple && ! empty(static::$_verified))
			{
				return true;
			}

			$i = $i instanceof Auth_Login_Driver ? $i : static::instance($i);
			if ( ! array_key_exists($i->get_id(), static::$_verified))
			{
				$i->check();
			}

			if ($specific)
			{
				if (array_key_exists($i->get_id(), static::$_verified))
				{
					return true;
				}
			}
		}

		return $specific === null && ! empty(static::$_verified);
	}

	/**
	 * Get verified driver or all verified drivers
	 * returns false when specific driver has not validated
	 * when all were requested and none validated an empty array is returned
	 *
	 * @param   null|string  driver id or null for all verified driver in an array
	 * @return  Array|Auth_Login_Driver|false
	 */
	public static function verified($driver = null)
	{
		if ($driver === null)
		{
			return static::$_verified;
		}

		if ( ! array_key_exists($driver, static::$_verified))
		{
			return false;
		}

		return static::$_verified[$driver];
	}

	/**
	 * Logs out all current logged in drivers
	 */
	public static function logout()
	{
		foreach (static::$_verified as $v)
		{
			$v->logout();
		}

		static::$_verified = array();
	}

	/**
	 * Register verified Login driver
	 *
	 * @param  Auth_Login_Driver
	 */
	public static function _register_verified(Auth_Login_Driver $driver)
	{
		static::$_verified[$driver->get_id()] = $driver;
	}

	/**
	 * Unregister verified Login driver
	 *
	 * @param  Auth_Login_Driver
	 */
	public static function _unregister_verified(Auth_Login_Driver $driver)
	{
		unset(static::$_verified[$driver->get_id()]);
	}

	/**
	 * Register a new driver type
	 *
	 * @param   string  name of the driver type, may not conflict with class method name
	 * @param   string  name of the method to use for checking this type of driver, also cannot conflict with method
	 * @return  bool
	 */
	public static function register_driver_type($type, $check_method)
	{
		$driver_exists = ! is_string($type)
						|| array_key_exists($type, static::$_drivers)
						|| method_exists(get_called_class(), $check_method)
						|| in_array($type, array('login', 'group', 'acl'));
		$method_exists = ! is_string($type)
						|| array_search($check_method, static::$_drivers)
						|| method_exists(get_called_class(), $type);

		if ($driver_exists && static::$_drivers[$type] == $check_method)
		{
			return true;
		}

		if ($driver_exists || $method_exists)
		{
			\Error::notice('Cannot add driver type, its name conflicts with another driver or method.');
			return false;
		}

		static::$_drivers[$type] = $check_method;
		return true;
	}

	/**
	 * Unregister a driver type
	 *
	 * @param   string  name of the driver type
	 * @return  bool
	 */
	public static function unregister_driver_type($type)
	{
		if (in_array('login', 'group', 'acl'))
		{
			\Error::notice('Cannot remove driver type, included drivers login, group and acl cannot be removed.');
			return false;
		}

		unset(static::$_drivers[$type]);
		return true;
	}

	/**
	 * Magic method used to retrieve driver instances and check them for validity
	 *
	 * @param   string
	 * @param   array
	 * @return  mixed
	 * @throws  BadMethodCallException
	 */
	public static function __callStatic($method, $args)
	{
		$args = array_pad($args, 3, null);
		if (array_key_exists($method, static::$_drivers))
		{
			return static::_driver_instance($method, $args[0]);
		}
		if ($type = array_search($method, static::$_drivers))
		{
			return static::_driver_check($type, $args[0], $args[1], @$args[2]);
		}
		if (static::$_verify_multiple !== true and method_exists(static::$_instance, $method))
		{
			return call_user_func_array(array(static::$_instance, $method), $args);
		}

		throw new \BadMethodCallException('Invalid method: '.get_called_class().'::'.$method);
	}

	/**
	 * Retrieve a loaded driver instance
	 * (loading must be done by other driver class)
	 *
	 * @param   string       driver type
	 * @param   string|true  driver id or true for an array of all loaded drivers
	 * @return  Auth_Driver|array
	 */
	protected static function _driver_instance($type, $instance)
	{
		$class = 'Auth_'.ucfirst($type).'_Driver';
		return $class::instance($instance);
	}

	/**
	 * Check driver
	 *
	 * @param   string  driver type
	 * @param   mixed   condition for which the driver is checked
	 * @param   string  driver id or null to check all
	 * @param   Array   identifier to check, should default to current user or relation therof and be
	 *                  in the form of array(driver_id, user_id)
	 * @return bool
	 */
	public static function _driver_check($type, $condition, $driver = null, $entity = null)
	{
		$method = static::$_drivers[$type];
		if ($driver === null)
		{
			if ($entity === null)
			{
				if ( ! empty(static::$_verified))
				{
					foreach (static::$_verified as $v)
					{
						if ($v->$method($condition))
						{
							return true;
						}
					}
				}
				else
				{
					foreach (static::$_instances as $i)
					{
						if ($i->guest_login() and $i->$method($condition))
						{
							return true;
						}
					}
				}
			}
			else
			{
				foreach (static::$_instances as $i)
				{
					if ($i->$method($condition, null, $entity))
					{
						return true;
					}
				}
			}
			return false;
		}
		else
		{
			if ($entity === null)
			{
				foreach (static::$_verified as $v)
				{
					if (static::$type($driver)->$method($condition))
					{
						return true;
					}
				}
			}
			elseif (static::$type($driver)->$method($condition, $entity))
			{
				return true;
			}

			return false;
		}
	}
}

/* end of file auth.php */
