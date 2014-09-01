<?php namespace Auth;
/**
 * Auth interface
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCAuth 
{
	/** 
	 * Get an auth handler
	 *
	 * @param string		$name	The auth instance
	 * @return bool
	 */
	public static function handler( $name = null )
	{
		return Handler::create( $name );
	}
	
	/** 
	 * Check if the login is valid
	 *
	 * @param string		$name	The auth instance
	 * @return bool
	 */
	public static function valid( $name = null )
	{
		return Handler::create( $name )->valid();
	}
	
	/** 
	 * Validate user credentials
	 *
	 * @param mixed		$identifier
	 * @param string		$password
	 * @param string		$name			The auth instance
	 * @return bool
	 */
	public static function validate( $identifier, $password, $name = null )
	{
		return Handler::create( $name )->validate( $identifier, $password );
	}
	
	/** 
	 * Sign in a user
	 *
	 * @param Auth\User		$user
	 * @param string			$keep_loggedin
	 * @param string			$name				The auth instance
	 * @return bool
	 */
	public static function sign_in( \Auth\User $user, $keep_loggedin = true, $name = null )
	{
		return Handler::create( $name )->sign_in( $user, $keep_loggedin );
	}
	
	/** 
	 * Sign out a user
	 *
	 * @param string			$name				The auth instance
	 * @return false
	 */
	public static function sign_out( $name = null )
	{
		return Handler::create( $name )->sign_out();
	}
}