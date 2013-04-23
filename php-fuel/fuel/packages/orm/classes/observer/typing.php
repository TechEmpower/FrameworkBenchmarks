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
 * Invalid content exception, thrown when type conversion is not possible.
 */
class InvalidContentType extends \UnexpectedValueException {}

/**
 * Typing observer.
 *
 * Runs on load or save, and ensures the correct data type of your ORM object properties.
 */
class Observer_Typing
{
	/**
	 * @var  array  types of events to act on and whether they are pre- or post-database
	 */
	public static $events = array(
		'before_save'  => 'before',
		'after_save'   => 'after',
		'after_load'   => 'after',
	);

	/**
	 * @var  array  regexes for db types with the method(s) to use, optionally pre- or post-database
	 */
	public static $type_methods = array(
		'/^varchar/uiD' => array(
			'before' => 'Orm\\Observer_Typing::type_string',
		),
		'/^(tiny|small|medium|big)?int(eger)?/uiD'
			=> 'Orm\\Observer_Typing::type_integer',
		'/^(float|double|decimal)/uiD'
			=> 'Orm\\Observer_Typing::type_float',
		'/^(tiny|medium|long)?text/' => array(
			'before' => 'Orm\\Observer_Typing::type_string',
		),
		'/^set/uiD' => array(
			'before' => 'Orm\\Observer_Typing::type_set_before',
			'after' => 'Orm\\Observer_Typing::type_set_after',
		),
		'/^enum/uiD' => array(
			'before' => 'Orm\\Observer_Typing::type_set_before',
		),
		'/^bool(ean)?$/uiD' => array(
			'before' => 'Orm\\Observer_Typing::type_bool_to_int',
			'after'  => 'Orm\\Observer_Typing::type_bool_from_int',
		),
		'/^serialize$/uiD' => array(
			'before' => 'Orm\\Observer_Typing::type_serialize',
			'after'  => 'Orm\\Observer_Typing::type_unserialize',
		),
		'/^json$/uiD' => array(
			'before' => 'Orm\\Observer_Typing::type_json_encode',
			'after'  => 'Orm\\Observer_Typing::type_json_decode',
		),
		'/^time_(unix|mysql)$/' => array(
			'before' => 'Orm\\Observer_Typing::type_time_encode',
			'after'  => 'Orm\\Observer_Typing::type_time_decode',
		),
	);

	/**
	 * Get notified of an event
	 *
	 * @param  Model   $instance
	 * @param  string  $event
	 */
	public static function orm_notify(Model $instance, $event)
	{
		if ( ! array_key_exists($event, static::$events))
		{
			return;
		}

		$event_type = static::$events[$event];
		$properties = $instance->properties();

		foreach ($properties as $p => $settings)
		{
			if (empty($settings['data_type']) || in_array($p, $instance->primary_key()))
			{
				continue;
			}
			if ($instance->{$p} === null) // add check if null is allowed
			{
				if (array_key_exists('null', $settings) and $settings['null'] === false)
				{
					throw new InvalidContentType('The property "'.$p.'" cannot be NULL.');
				}
				continue;
			}

			foreach (static::$type_methods as $match => $method)
			{
				if (is_array($method))
				{
					$method = ! empty($method[$event_type]) ? $method[$event_type] : false;
				}
				if ($method === false)
				{
					continue;
				}

				if ($method and preg_match($match, $settings['data_type']) > 0)
				{
					$instance->{$p} = call_user_func($method, $instance->{$p}, $settings);
					continue;
				}
			}
		}

		if ($event_type == 'after')
		{
			$instance->_update_original();
		}
	}

	/**
	 * Typecast a single column value based on the model properties for that column
	 *
	 * @param  string  $column	name of the column
	 * @param  string  $value	value
	 * @param  string  $settings	column settings from the model
	 *
	 * @throws  InvalidContentType
	 *
	 * @return  mixed
	 */
	public static function typecast($column, $value, $settings)
	{
		if ($value === null) // add check if null is allowed
		{
			if (array_key_exists('null', $settings) and $settings['null'] === false)
			{
				throw new InvalidContentType('The property "'.$column.'" cannot be NULL.');
			}
		}

		if (isset($settings['data_type']))
		{
			foreach (static::$type_methods as $match => $method)
			{
				if (is_array($method))
				{
					if ( ! empty($method['before']))
					{
						$method = $method['before'];
					}
					else
					{
						continue;
					}
				}
				if ($method and preg_match($match, $settings['data_type']) > 0)
				{
					$value = call_user_func($method, $value, $settings);
					break;
				}
			}
		}

		return $value;
	}

	/**
	 * Casts to string when necessary and checks if within max length
	 *
	 * @param   mixed  value to typecast
	 * @param   array  any options to be passed
	 *
	 * @throws  InvalidContentType
	 *
	 * @return  string
	 */
	public static function type_string($var, array $settings)
	{
		if (is_array($var) or (is_object($var) and ! method_exists($var, '__toString')))
		{
			throw new InvalidContentType('Array or object could not be converted to varchar.');
		}

		$var = strval($var);

		if (array_key_exists('character_maximum_length', $settings))
		{
			$length  = intval($settings['character_maximum_length']);
			if ($length > 0 and strlen($var) > $length)
			{
				$var = substr($var, 0, $length);
			}
		}

		return $var;
	}

	/**
	 * Casts to int when necessary and checks if within max values
	 *
	 * @param   mixed  value to typecast
	 * @param   array  any options to be passed
	 *
	 * @throws  InvalidContentType
	 *
	 * @return  int
	 */
	public static function type_integer($var, array $settings)
	{
		if (is_array($var) or is_object($var))
		{
			throw new InvalidContentType('Array or object could not be converted to integer.');
		}

		if ((array_key_exists('min', $settings) and $var < intval($settings['min']))
			or (array_key_exists('max', $settings) and $var > intval($settings['max'])))
		{
			throw new InvalidContentType('Integer value outside of range: '.$var);
		}

		return intval($var);
	}

	/**
	 * Casts to float when necessary
	 *
	 * @param   mixed  value to typecast
	 *
	 * @throws  InvalidContentType
	 *
	 * @return  float
	 */
	public static function type_float($var)
	{
		if (is_array($var) or is_object($var))
		{
			throw new InvalidContentType('Array or object could not be converted to float.');
		}

		return floatval($var);
	}

	/**
	 * Value pre-treater, deals with array values, and handles the enum type
	 *
	 * @param   mixed  value
	 * @param   array  any options to be passed
	 *
	 * @throws  InvalidContentType
	 *
	 * @return  string
	 */
	public static function type_set_before($var, array $settings)
	{
		$var    = is_array($var) ? implode(',', $var) : strval($var);
		$values = array_filter(explode(',', trim($var)));

		if ($settings['data_type'] == 'enum' and count($values) > 1)
		{
			throw new InvalidContentType('Enum cannot have more than 1 value.');
		}

		foreach ($values as $val)
		{
			if ( ! in_array($val, $settings['options']))
			{
				throw new InvalidContentType('Invalid value given for '.ucfirst($settings['data_type']).
					', value "'.$var.'" not in available options: "'.implode(', ', $settings['options']).'".');
			}
		}

		return $var;
	}

	/**
	 * Value post-treater, converts a comma-delimited string into an array
	 *
	 * @param   mixed  value
	 *
	 * @return  array
	 */
	public static function type_set_after($var)
	{
		return explode(',', $var);
	}

	/**
	 * Converts boolean input to 1 or 0 for the DB
	 *
	 * @param   bool  value
	 *
	 * @return  int
	 */
	public static function type_bool_to_int($var)
	{
		return $var ? 1 : 0;
	}

	/**
	 * Converts DB bool values to PHP bool value
	 *
	 * @param   bool  value
	 *
	 * @return  int
	 */
	public static function type_bool_from_int($var)
	{
		return $var == '1' ? true : false;
	}

	/**
	 * Returns the serialized input
	 *
	 * @param   mixed  value
	 * @param   array  any options to be passed
	 *
	 * @throws  InvalidContentType
	 *
	 * @return  string
	 */
	public static function type_serialize($var, array $settings)
	{
		$var = serialize($var);

		if (array_key_exists('character_maximum_length', $settings))
		{
			$length  = intval($settings['character_maximum_length']);
			if ($length > 0 and strlen($var) > $length)
			{
				throw new InvalidContentType('Value could not be serialized, exceeds max string length for field.');
			}
		}

		return $var;
	}

	/**
	 * Unserializes the input
	 *
	 * @param   string  value
	 *
	 * @return  mixed
	 */
	public static function type_unserialize($var)
	{
		return unserialize($var);
	}

	/**
	 * JSON encodes the input
	 *
	 * @param   mixed  value
	 * @param   array  any options to be passed
	 *
	 * @throws  InvalidContentType
	 *
	 * @return  string
	 */
	public static function type_json_encode($var, array $settings)
	{
		$var = json_encode($var);

		if (array_key_exists('character_maximum_length', $settings))
		{
			$length  = intval($settings['character_maximum_length']);
			if ($length > 0 and strlen($var) > $length)
			{
				throw new InvalidContentType('Value could not be JSON encoded, exceeds max string length for field.');
			}
		}

		return $var;
	}

	/**
	 * Decodes the JSON
	 *
	 * @param   string  value
	 *
	 * @return  mixed
	 */
	public static function type_json_decode($var, $settings)
	{
		$assoc = false;
		if (array_key_exists('json_assoc', $settings))
		{
			$assoc = (bool)$settings['json_assoc'];
		}
		return json_decode($var, $assoc);
	}

	/**
	 * Takes a Date instance and transforms it into a DB timestamp
	 *
	 * @param   \Fuel\Core\Date  value
	 * @param   array  any options to be passed
	 *
	 * @throws  InvalidContentType
	 *
	 * @return  int|string
	 */
	public static function type_time_encode(\Fuel\Core\Date $var, array $settings)
	{
		if ( ! $var instanceof \Fuel\Core\Date)
		{
			throw new InvalidContentType('Value must be an instance of the Date class.');
		}

		if ($settings['data_type'] == 'time_mysql')
		{
			return $var->format('mysql');
		}

		return $var->get_timestamp();
	}

	/**
	 * Takes a DB timestamp and converts it into a Date object
	 *
	 * @param   string  value
	 * @param   array  any options to be passed
	 *
	 * @return  \Fuel\Core\Date
	 */
	public static function type_time_decode($var, array $settings)
	{
		if ($settings['data_type'] == 'time_mysql')
		{
			return \Date::create_from_string($var, 'mysql');
		}

		return \Date::forge($var);
	}
}
