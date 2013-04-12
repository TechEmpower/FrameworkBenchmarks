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


abstract class Auth_Acl_Driver extends \Auth_Driver
{

	/**
	 * @var	Auth_Driver	default instance
	 */
	protected static $_instance = null;

	/**
	 * @var	array	contains references if multiple were loaded
	 */
	protected static $_instances = array();

	public static function forge(array $config = array())
	{
		// default driver id to driver name when not given
		! array_key_exists('id', $config) && $config['id'] = $config['driver'];

		$class = \Inflector::get_namespace($config['driver']).'Auth_Acl_'.ucfirst(\Inflector::denamespace($config['driver']));
		$driver = new $class($config);
		static::$_instances[$driver->get_id()] = $driver;
		is_null(static::$_instance) and static::$_instance = $driver;

		foreach ($driver->get_config('drivers', array()) as $type => $drivers)
		{
			foreach ($drivers as $d => $custom)
			{
				$custom = is_int($d)
					? array('driver' => $custom)
					: array_merge($custom, array('driver' => $d));
				$class = 'Auth_'.ucfirst($type).'_Driver';
				$class::forge($custom);
			}
		}

		return $driver;
	}

	/**
	 * Parses a conditions string into it's array equivalent
	 *
	 * @rights	mixed	conditions array or string
	 * @return	array	conditions array formatted as array(area, rights)
	 *
	 */
	public static function _parse_conditions($rights)
	{
		if (is_array($rights))
		{
			return $rights;
		}

		if ( ! is_string($rights) or strpos($rights, '.') === false)
		{
			throw new \InvalidArgumentException('Given rights where not formatted proppery. Formatting should be like area.right or area.[right, other_right]. Received: '.$rights);
		}

		list($area, $rights) = explode('.', $rights);

		if (substr($rights, 0, 1) == '[' and substr($rights, -1, 1) == ']')
		{
			$rights = preg_split('#( *)?,( *)?#', trim(substr($rights, 1, -1)));
		}

		return array($area, $rights);
	}

	// ------------------------------------------------------------------------

	/**
	 * Check access rights
	 *
	 * @param	mixed	condition to check for access
	 * @param	mixed	user or group identifier in the form of array(driver_id, id)
	 * @return	bool
	 */
	abstract public function has_access($condition, Array $entity);
}

/* end of file driver.php */
