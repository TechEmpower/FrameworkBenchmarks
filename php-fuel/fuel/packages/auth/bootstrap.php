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


Autoloader::add_core_namespace('Auth');

Autoloader::add_classes(array(
	'Auth\\Auth'           => __DIR__.'/classes/auth.php',
	'Auth\\AuthException'  => __DIR__.'/classes/auth.php',

	'Auth\\Auth_Driver'  => __DIR__.'/classes/auth/driver.php',

	'Auth\\Auth_Acl_Driver'     => __DIR__.'/classes/auth/acl/driver.php',
	'Auth\\Auth_Acl_SimpleAcl'  => __DIR__.'/classes/auth/acl/simpleacl.php',

	'Auth\\Auth_Group_Driver'       => __DIR__.'/classes/auth/group/driver.php',
	'Auth\\Auth_Group_SimpleGroup'  => __DIR__.'/classes/auth/group/simplegroup.php',

	'Auth\\Auth_Login_Driver'          => __DIR__.'/classes/auth/login/driver.php',
	'Auth\\Auth_Login_SimpleAuth'      => __DIR__.'/classes/auth/login/simpleauth.php',
	'Auth\\SimpleUserUpdateException'  => __DIR__.'/classes/auth/login/simpleauth.php',
	'Auth\\SimpleUserWrongPassword'    => __DIR__.'/classes/auth/login/simpleauth.php',
));


/* End of file bootstrap.php */
