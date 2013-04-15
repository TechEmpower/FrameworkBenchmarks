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


abstract class Auth_Driver
{

	/**
	 * @var	Auth_Driver
	 * THIS MUST BE DEFINED IN THE BASE EXTENSION
	 */
	// protected static $_instance = null;

	/**
	 * @var	array	contains references if multiple were loaded
	 * THIS MUST BE DEFINED IN THE BASE EXTENSION
	 */
	// protected static $_instances = array();

	public static function forge(array $config = array())
	{
		throw new \AuthException('Driver must have a factory method extension.');
	}

	/**
	 * Return a specific driver, or the default instance
	 *
	 * @param	string	driver id
	 * @return	Auth_Driver
	 */
	public static function instance($instance = null)
	{
		if ($instance === true)
		{
			return static::$_instances;
		}
		elseif ($instance !== null)
		{
			if ( ! array_key_exists($instance, static::$_instances))
			{
				return false;
			}

			return static::$_instances[$instance];
		}

		return static::$_instance;
	}

	// ------------------------------------------------------------------------

	/**
	 * @var	string	instance identifier
	 */
	protected $id;

	/**
	 * @var	array	given configuration array
	 */
	protected $config = array();

	protected function __construct(Array $config)
	{
		$this->id = $config['id'];
		$this->config = array_merge($this->config, $config);
	}

	/**
	 * Get driver instance ID
	 *
	 * @return string
	 */
	public function get_id()
	{
		return (string) $this->id;
	}

	/**
	 * Create or change config value
	 *
	 * @param	string
	 * @param	mixed
	 */
	public function set_config($key, $value)
	{
		$this->config[$key] = $value;
	}

	/**
	 * Retrieve config value
	 *
	 * @param	string
	 * @param	mixed	return when key doesn't exist
	 * @return	mixed
	 */
	public function get_config($key, $default = null)
	{
		return array_key_exists($key, $this->config) ? $this->config[$key] : $default;
	}

	/**
	 * Whether this driver supports guest login
	 *
	 * @return  bool
	 */
	public function guest_login()
	{
		return false;
	}
}

/* end of file driver.php */
