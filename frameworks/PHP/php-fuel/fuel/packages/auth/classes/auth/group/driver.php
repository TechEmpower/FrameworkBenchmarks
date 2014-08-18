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


abstract class Auth_Group_Driver extends \Auth_Driver
{

	/**
	 * @var	Auth_Driver
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

		$class = \Inflector::get_namespace($config['driver']).'Auth_Group_'.ucfirst(\Inflector::denamespace($config['driver']));
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

	// ------------------------------------------------------------------------

	/**
	 * Verify Acl access
	 *
	 * @param	mixed	condition to validate
	 * @param	string	acl driver id or null to check all
	 * @param	array	user identifier to check in form array(driver_id, user_id)
	 * @return	bool
	 */
	public function has_access($condition, $driver, $group = null)
	{
		// When group was given just check the group
		if (is_array($group))
		{
			if ($driver === null)
			{
				foreach (\Auth::acl(true) as $acl)
				{
					if ($acl->has_access($condition, $group))
					{
						return true;
					}
				}

				return false;
			}

			return \Auth::acl($driver)->has_access($condition, $group);
		}

		// When no group was given check all logged in users
		foreach (\Auth::verified() as $v)
		{
			// ... and check all those their groups
			$gs = $v->get_groups();
			foreach ($gs as $g_id)
			{
				// ... and try to validate if its group is this one
				if ($this instanceof $g_id[0])
				{
					if ($this->has_access($condition, $driver, $g_id))
					{
						return true;
					}
				}
			}
		}

		// when nothing validated yet: it has failed to
		return false;
	}

	// ------------------------------------------------------------------------

	/**
	 * Check membership of given users
	 *
	 * @param	mixed	condition to check for access
	 * @param	array	user identifier in the form of array(driver_id, user_id), or null for logged in
	 * @return	bool
	 */
	abstract public function member($group, $user = null);

	/**
	 * Fetch the display name of the given group
	 *
	 * @param	mixed	group condition to check
	 * @return	string
	 */
	abstract public function get_name($group);
}

/* end of file driver.php */
