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


class Auth_Acl_SimpleAcl extends \Auth_Acl_Driver
{

	protected static $_valid_roles = array();

	public static function _init()
	{
		static::$_valid_roles = array_keys(\Config::get('simpleauth.roles'));
	}

	public function has_access($condition, Array $entity)
	{
		$group = \Auth::group($entity[0]);

		$condition = static::_parse_conditions($condition);

		if ( ! is_array($condition) || empty($group) || ! is_callable(array($group, 'get_roles')))
		{
			return false;
		}

		$area    = $condition[0];
		$rights  = (array) $condition[1];
		$current_roles  = $group->get_roles($entity[1]);
		$current_rights = array();
		if (is_array($current_roles))
		{
			$roles = \Config::get('simpleauth.roles', array());
			array_key_exists('#', $roles) && array_unshift($current_roles, '#');
			foreach ($current_roles as $r_role)
			{
				// continue if the role wasn't found
				if ( ! array_key_exists($r_role, $roles))
				{
					continue;
				}
				$r_rights = $roles[$r_role];

				// if one of the roles has a negative or positive wildcard return it without question
				if (is_bool($r_rights))
				{
					return $r_rights;
				}
				// if there are roles for the current area, merge them with earlier fetched roles
				elseif (array_key_exists($area, $r_rights))
				{
					$current_rights = array_unique(array_merge($current_rights, $r_rights[$area]));
				}
			}
		}

		// start checking rights, terminate false when right not found
		foreach ($rights as $right)
		{
			if ( ! in_array($right, $current_rights))
			{
				return false;
			}
		}

		// all necessary rights were found, return true
		return true;
	}
}

/* end of file simpleacl.php */
