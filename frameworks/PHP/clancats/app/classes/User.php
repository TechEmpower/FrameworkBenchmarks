<?php
/**
 * User model
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class User extends Auth\User
{	
	/**
	 * The users table
	 */
	protected static $_table = 'auth_users';
	
	/**
	 * The user model defaults
	 *
	 * @var array
	 */
	protected static $_defaults = array(
		'id'	,
		'active'			=> array( 'bool', true ),
		'group_id'		=> 0,
		'email'			=> null,
		'password'		=> null,
		'storage'		=> array( 'json', array() ),
		'last_login'		=> array( 'timestamp', 0 ),
		'created_at'		=> array( 'timestamp' ),
		'modified_at'	=> array( 'timestamp' ),
	);
}