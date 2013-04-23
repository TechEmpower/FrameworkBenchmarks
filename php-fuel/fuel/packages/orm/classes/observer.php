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
 * Observer base class
 */
abstract class Observer
{
	/**
	 * @var	array	list of created observer instances created
	 */
	protected static $_instances = array();

	/**
	 * Get notified of an event
	 *
	 * @param  Model   $instance
	 * @param  string  $event
	 */
	public static function orm_notify($instance, $event)
	{
		$model_class = get_class($instance);
		if (method_exists(static::instance($model_class), $event))
		{
			static::instance($model_class)->{$event}($instance);
		}
	}

	/**
	 * Create an instance of this observer
	 *
	 * @param  string  name of the model class
	 */
	public static function instance($model_class)
	{
		$observer = get_called_class();
		if (empty(static::$_instances[$observer][$model_class]))
		{
			static::$_instances[$observer][$model_class] = new static($model_class);
		}

		return static::$_instances[$observer][$model_class];
	}
}
