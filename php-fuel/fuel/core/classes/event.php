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
 * Event Class
 *
 * @package		Fuel
 * @category	Core
 * @author		Eric Barnes
 * @author		Harro "WanWizard" Verton
 */
abstract class Event
{
	/**
	 * @var  array  $instances  Event_Instance container
	 */
	protected static $instances = array();

	/**
	 * Event instance forge.
	 *
	 * @param   array   $events  events array
	 * @return  object  new Event_Instance instance
	 */
	public static function forge(array $events = array())
	{
		return new \Event_Instance($events);
	}

	/**
	 * Multiton Event instance.
	 *
	 * @param   string  $name    instance name
	 * @param   array   $events  events array
	 * @return  object  Event_Instance object
	 */
	public static function instance($name = 'fuelphp', array $events = array())
	{
		if ( ! array_key_exists($name, static::$instances))
		{
			$events = array_merge(\Config::get('event.'.$name, array()), $events);
			$instance = static::forge($events);
			static::$instances[$name] = &$instance;
		}

		return static::$instances[$name];
	}

	// --------------------------------------------------------------------

	/**
	 * method called by register_shutdown_event
	 *
	 * @access	public
	 * @param	void
	 * @return	void
	 */
	public static function shutdown()
	{
		$instance = static::instance();
		if ($instance->has_events('shutdown'))
		{
			// trigger the shutdown events
			$instance->trigger('shutdown', '', 'none', true);
		}
	}

	/**
	 * Static call forwarder
	 *
	 * @param   string  $func  method name
	 * @param   array   $args  passed arguments
	 * @return
	 */
	public static function __callStatic($func, $args)
	{
		$instance = static::instance();

		if (method_exists($instance, $func))
		{
			return call_user_func_array(array($instance, $func), $args);
		}

		throw new \BadMethodCallException('Call to undefined method: '.get_called_class().'::'.$func);
	}

	/**
	 * Load events config
	 */
	public static function _init()
	{
		\Config::load('event', true);
	}
}
