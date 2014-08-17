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

namespace Orm;

/**
 * Dummy observer class, which allows you to define observer methods in the
 * model itself.
 */
class Observer_Self
{
	/**
	 * Get notified of an event
	 *
	 * @param  Model   $instance
	 * @param  string  $event
	 */
	public static function orm_notify(Model $instance, $event)
	{
		if (method_exists($instance, $method = '_event_'.$event))
		{
			call_user_func(array($instance, $method));
		}
	}
}
