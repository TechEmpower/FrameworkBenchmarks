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

/**
 * Session Class
 *
 * @package		Fuel
 * @category	Core
 * @author		Harro "WanWizard" Verton
 * @link		http://docs.fuelphp.com/classes/session.html
 */
class Session
{
	/**
	 * loaded session driver instance
	 */
	protected static $_instance = null;

	/**
	 * array of loaded instances
	 */
	protected static $_instances = array();

	/**
	 * array of global config defaults
	 */
	protected static $_defaults = array(
		'driver'                    => 'cookie',
		'match_ip'                  => false,
		'match_ua'                  => true,
		'cookie_domain'             => '',
		'cookie_path'               => '/',
		'cookie_http_only'          => null,
		'encrypt_cookie'            => true,
		'expire_on_close'           => false,
		'expiration_time'           => 7200,
		'rotation_time'             => 300,
		'flash_id'                  => 'flash',
		'flash_auto_expire'         => true,
		'flash_expire_after_get'    => true,
		'post_cookie_name'          => ''
	);

	// --------------------------------------------------------------------

	/**
	 * Initialize by loading config & starting default session
	 */
	public static function _init()
	{
		\Config::load('session', true);

		if (\Config::get('session.auto_initialize', true))
		{
			static::instance();
		}
	}

	// --------------------------------------------------------------------

	/**
	 * Factory
	 *
	 * Produces fully configured session driver instances
	 *
	 * @param	array|string	full driver config or just driver type
	 */
	public static function forge($custom = array())
	{
		$config = \Config::get('session', array());

		// When a string was passed it's just the driver type
		if ( ! empty($custom) and ! is_array($custom))
		{
			$custom = array('driver' => $custom);
		}

		$config = array_merge(static::$_defaults, $config, $custom);

		if (empty($config['driver']))
		{
			throw new \Session_Exception('No session driver given or no default session driver set.');
		}

		// determine the driver to load
		$class = '\\Session_'.ucfirst($config['driver']);

		$driver = new $class($config);

		// get the driver's cookie name
		$cookie = $driver->get_config('cookie_name');

		// do we already have a driver instance for this cookie?
		if (isset(static::$_instances[$cookie]))
		{
			// if so, they must be using the same driver class!
			$class_instance = 'Fuel\\Core\\'.$class;
			if (static::$_instances[$cookie] instanceof $class_instance)
			{
				throw new \FuelException('You can not instantiate two different sessions using the same cookie name "'.$cookie.'"');
			}
		}
		else
		{
			// register a shutdown event to update the session
			\Event::register('shutdown', array($driver, 'write'));

			// init the session
			$driver->init();
			$driver->read();

			// store this instance
			static::$_instances[$cookie] =& $driver;
		}

		return static::$_instances[$cookie];
	}

	// --------------------------------------------------------------------

	/**
	 * class constructor
	 *
	 * @param	void
	 * @access	private
	 * @return	void
	 */
	final private function __construct() {}

	// --------------------------------------------------------------------

	/**
	 * create or return the driver instance
	 *
	 * @param	void
	 * @access	public
	 * @return	Session_Driver object
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

	// --------------------------------------------------------------------

	/**
	 * set session variables
	 *
	 * @param	string|array	name of the variable to set or array of values, array(name => value)
	 * @param	mixed			value
	 * @access	public
	 * @return	void
	 */
	public static function set($name, $value = null)
	{
		return static::instance()->set($name, $value);
	}

	// --------------------------------------------------------------------

	/**
	 * get session variables
	 *
	 * @access	public
	 * @param	string	name of the variable to get
	 * @param	mixed	default value to return if the variable does not exist
	 * @return	mixed
	 */
	public static function get($name = null, $default = null)
	{
		return static::instance()->get($name, $default);
	}

	// --------------------------------------------------------------------

	/**
	 * delete a session variable
	 *
	 * @param	string	name of the variable to delete
	 * @param	mixed	value
	 * @access	public
	 * @return	void
	 */
	public static function delete($name)
	{
		return static::instance()->delete($name);
	}

	// --------------------------------------------------------------------

	/**
	 * get session key variables
	 *
	 * @access	public
	 * @param	string	name of the variable to get, default is 'session_id'
	 * @return	mixed
	 */
	public static function key($name = 'session_id')
	{
		return static::instance()->key($name);
	}

	// --------------------------------------------------------------------

	/**
	 * set session flash variables
	 *
	 * @param	string	name of the variable to set
	 * @param	mixed	value
	 * @access	public
	 * @return	void
	 */
	public static function set_flash($name, $value = null)
	{
		return static::instance()->set_flash($name, $value);
	}

	// --------------------------------------------------------------------

	/**
	 * get session flash variables
	 *
	 * @access	public
	 * @param	string	name of the variable to get
	 * @param	mixed	default value to return if the variable does not exist
	 * @param	bool	true if the flash variable needs to expire immediately
	 * @return	mixed
	 */
	public static function get_flash($name = null, $default = null, $expire = null)
	{
		return static::instance()->get_flash($name, $default, $expire);
	}

	// --------------------------------------------------------------------

	/**
	 * keep session flash variables
	 *
	 * @access	public
	 * @param	string	name of the variable to keep
	 * @return	void
	 */
	public static function keep_flash($name = null)
	{
		return static::instance()->keep_flash($name);
	}

	// --------------------------------------------------------------------

	/**
	 * delete session flash variables
	 *
	 * @param	string	name of the variable to delete
	 * @param	mixed	value
	 * @access	public
	 * @return	void
	 */
	public static function delete_flash($name = null)
	{
		return static::instance()->delete_flash($name);
	}

	// --------------------------------------------------------------------

	/**
	 * create a new session
	 *
	 * @access	public
	 * @return	void
	 */
	public static function create()
	{
		return static::instance()->create();
	}

	// --------------------------------------------------------------------

	/**
	 * read the session
	 *
	 * @access	public
	 * @return	void
	 */
	public static function read()
	{
		return static::instance()->read();
	}

	// --------------------------------------------------------------------

	/**
	 * write the session
	 *
	 * @access	public
	 * @return	void
	 */
	public static function write()
	{
		return static::instance()->write();
	}

	// --------------------------------------------------------------------

	/**
	 * rotate the session id
	 *
	 * @access	public
	 * @return	void
	 */
	public static function rotate()
	{
		return static::instance()->rotate();
	}

	// --------------------------------------------------------------------

	/**
	 * destroy the current session
	 *
	 * @access	public
	 * @return	void
	 */
	public static function destroy()
	{
		return static::instance()->destroy();
	}

}


