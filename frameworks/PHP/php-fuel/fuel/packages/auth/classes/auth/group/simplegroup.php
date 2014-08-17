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


class Auth_Group_SimpleGroup extends \Auth_Group_Driver
{

	public static $_valid_groups = array();

	public static function _init()
	{
		static::$_valid_groups = array_keys(\Config::get('simpleauth.groups', array()));
	}

	protected $config = array(
		'drivers' => array('acl' => array('SimpleAcl'))
	);

	public function member($group, $user = null)
	{
		if ($user === null)
		{
			$groups = \Auth::instance()->get_groups();
		}
		else
		{
			$groups = \Auth::instance($user[0])->get_groups();
		}

		if ( ! $groups || ! in_array((int) $group, static::$_valid_groups))
		{
			return false;
		}

		return in_array(array($this->id, $group), $groups);
	}

	public function get_name($group = null)
	{
		if ($group === null)
		{
			if ( ! $login = \Auth::instance() or ! is_array($groups = $login->get_groups()))
			{
				return false;
			}
			$group = isset($groups[0][1]) ? $groups[0][1] : null;
		}

		return \Config::get('simpleauth.groups.'.$group.'.name', null);
	}

	public function get_roles($group = null)
	{
		// When group is empty, attempt to get groups from a current login
		if ($group === null)
		{
			if ( ! $login = \Auth::instance()
				or ! is_array($groups = $login->get_groups())
				or ! isset($groups[0][1]))
			{
				return array();
			}
			$group = $groups[0][1];
		}
		elseif ( ! in_array((int) $group, static::$_valid_groups))
		{
			return array();
		}

		$groups = \Config::get('simpleauth.groups');
		return $groups[(int) $group]['roles'];
	}
}

/* end of file simplegroup.php */
