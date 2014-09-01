<?php namespace Core;
/**
 * Cookie handler
 * @TODO: This is a simple port to CCF2.0 from v1. Cookies should be added
 * as header to the CCResposne instead of unsing the php native functions.
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCCookie 
{	
	/*
	 * Cookie Path
	 */
	public static $path = '/';
	
	/*
	 * Cookie Domain
	 */
	public static $domain = null;
	
	/*
	 * Secure
	 */
	public static $secure = false;
	
	/*
	 * HTTP only
	 */
	public static $httponly = false;
	
	
	/**
	 * Set a cookie
	 * 
	 * @param string 	$key
	 * @param string 	$value
	 * @param int		$expire
	 * @return bool
	 */
	public static function set( $key, $value, $expire = 0 ) {
		
		// set the expire date 
		if ( $expire > 0 ) {
			
			if ( $expire < time() ) {
				$expire = time() + $expire;
			}
		}
		
		CCServer::$_instance->COOKIE[$key] = $value;
		
		/*
		 * Finally set the cookie
		 * @toDo: just at the cookie to an array an set the set cookie header from the CCResponse
		 */
		return setcookie( 
			$key, 
			$value, 
			$expire, 
			static::$path, 
			static::$domain, 
			static::$secure, 
			static::$httponly 
		);
	}
	
	/**
	 * get a cookie 
	 * 
	 * @param string 	$key
	 * @return mixed
	 */
	public static function get( $key, $default = null ) 
	{
		if ( !isset( CCServer::$_instance->COOKIE[$key] ) ) 
		{
			return $default;
		}
		return CCServer::$_instance->COOKIE[$key];
	}
	
	/**
	 * eat a cookie, means get it once
	 * 
	 * @param string 	$key
	 * @return mixed
	 */
	public static function once( $key ) {
		if ( !is_null( static::get( $key ) ) ) {
				
			$cookie = static::get( $key );
			static::delete( $key );
			
			return $cookie;
		}
	}
	
	/**
	 * has a cookie
	 *
	 * @param string 	$key
	 * @return bool
	 */
	public static function has( $key ) {
		return isset( CCServer::$_instance->COOKIE[$key] );
	}
	
	
	/**
	 * delete a cookie
	 *
	 * @param string 	$key
	 * @return bool
	 */
	public static function delete( $key ) 
	{
		unset( CCServer::$_instance->COOKIE[$key] );
		return setcookie( $key, "REMOVED BY CHUCK NORRIS!", time()-1000 );
	}
}