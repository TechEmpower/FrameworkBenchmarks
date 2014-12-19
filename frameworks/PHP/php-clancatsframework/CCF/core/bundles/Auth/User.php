<?php namespace Auth;
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
class User extends \DB\Model
{
	/**
	 * Hidden fields
	 */
	protected static $_hidden = array( 'password' );
	
	/*
	 * Let the model automatically set created_at and modified_at 
	 */
	protected static $_timestamps = true;
	
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
	
	/**
	 * Get the user group
	 *
	 * @return \Auth\Group
	 */
	public function group()
	{
		return $this->belongs_to( '\\Auth\\Group' );
	}
	
	/**
	 * Is the user an administrator?
	 * We simply check the group_id in our default configuration
	 * the user id of the admin group is 100.
	 *
	 * @return bool
	 */
	public function is_admin()
	{
		return $this->group_id == 100;
	}
	
	/**
	 * Always hash the passwort 
	 * 
	 * @param string 		$password
	 * @return string
	 */
	protected function _set_modifier_password( $password )
	{
		return \CCStr::hash( $password );
	}
	
	/**
	 * The email should only contain lower case characters
	 * 
	 * @param string 		$email
	 * @return string
	 */
	protected function _set_modifier_email( $email )
	{
		return strtolower( $email );
	}
}