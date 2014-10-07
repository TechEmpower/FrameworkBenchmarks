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

namespace Email;

class AttachmentNotFoundException extends \FuelException {}

class InvalidAttachmentsException extends \FuelException {}

class InvalidEmailStringEncoding extends \FuelException {}

class EmailSendingFailedException extends \FuelException {}

class EmailValidationFailedException extends \FuelException {}

class Email
{

	/**
	 * Instance for singleton usage.
	 */
	public static $_instance = false;

	/**
	 * Driver config defaults.
	 */
	protected static $_defaults;

	/**
	 * Email priorities
	 */
	const P_LOWEST		= '5 (Lowest)';
	const P_LOW			= '4 (Low)';
	const P_NORMAL		= '3 (Normal)';
	const P_HIGH		= '2 (High)';
	const P_HIGHEST		= '1 (Highest)';

	/**
	 * Email driver forge.
	 *
	 * @param	string|array	$setup		setup key for array defined in email.setups config or config array
	 * @param	array			$config		extra config array
	 * @return  Email_Driver    one of the email drivers
	 */
	public static function forge($setup = null, array $config = array())
	{
		empty($setup) and $setup = \Config::get('email.default_setup', 'default');
		is_string($setup) and $setup = \Config::get('email.setups.'.$setup, array());

		$setup = \Arr::merge(static::$_defaults, $setup);
		$config = \Arr::merge($setup, $config);

		$driver = '\\Email_Driver_'.ucfirst(strtolower($config['driver']));

		if( ! class_exists($driver, true))
		{
			throw new \FuelException('Could not find Email driver: '.$config['driver']. ' ('.$driver.')');
		}

		$driver = new $driver($config);

		return $driver;
	}

	/**
	 * Init, config loading.
	 */
	public static function _init()
	{
		\Config::load('email', true);
		static::$_defaults = \Config::get('email.defaults');
	}

	/**
	 * Call rerouting for static usage.
	 *
	 * @param	string	$method		method name called
	 * @param	array	$args		supplied arguments
	 */
	public static function __callStatic($method, $args = array())
	{
		if(static::$_instance === false)
		{
			$instance = static::forge();
			static::$_instance = &$instance;
		}

		if(is_callable(array(static::$_instance, $method)))
		{
			return call_user_func_array(array(static::$_instance, $method), $args);
		}

		throw new \BadMethodCallException('Invalid method: '.get_called_class().'::'.$method);
	}

}
