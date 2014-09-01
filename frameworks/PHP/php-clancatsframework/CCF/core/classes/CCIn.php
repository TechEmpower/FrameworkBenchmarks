<?php namespace Core;
/**
 * Input
 *
 * This class mostly forwards the instance functions of the
 * CCIn_Instance object.
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCIn {
	
	/**
	 * the instance holder
	 */
	public static $_instance = null;
	
	/**
	 * get the current input instance
	 *
	 * @param CCIn_Instance		$set		if set the current instance gets updated
	 *
	 * @return CCIn_Instance
	 */
	public static function instance( $set = null ) 
	{
		if ( is_null( $set ) ) 
		{
			return static::$_instance;
		}
		
		if ( !$set instanceof CCIn_Instance ) 
		{
			throw new \InvalidArgumentException('CCIn::set() - only CCIn_Instance object can be passed.');
		}
		
		static::$_instance = $set;
	}
	
	/**
	 * create new instance
	 * assign the main vars GET, POST, COOKIE, FILES, SERVER here
	 * 
	 * @param array 		$get
	 * @param array 		$post
	 * @param array 		$cookie
	 * @param array 		$files
	 * @param array 		$server
	 */
	public static function create( $get, $post, $cookie, $files, $server ) 
	{
		return new CCIn_Instance( $get, $post, $cookie, $files, $server );
	}
	
	/**
	 * Return all data of a type
	 * 
	 * @param string 		$key
	 * @return array
	 */
	public static function all( $key )
	{
		$key = strtoupper( $key );	
		return static::$_instance->$key;
	}
	
	/**
	 * get a GET param
	 *
	 * @param string		$key
	 * @param mixed		$default
	 * @return mixed
	 */
	public static function get( $key, $default = null ) 
	{
		return static::$_instance->get( $key, $default );
	}
	
	/**
	 * has a GET param
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public static function has_get( $key ) 
	{
		return static::$_instance->has_get( $key );
	}
	
	/**
	 * get a POST param
	 *
	 * @param string		$key
	 * @param mixed		$default
	 * @return mixed
	 */
	public static function post( $key, $default = null ) 
	{
		return static::$_instance->post( $key, $default );
	}
	
	/**
	 * has a POST param
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public static function has_post( $key ) 
	{
		return static::$_instance->has_post( $key );
	}
	
	/**
	 * get a SERVER param
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public static function server( $key, $default = null ) 
	{
		return static::$_instance->server( $key, $default );
	}
	
	/**
	 * has a SERVER param
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public static function has_server( $key ) 
	{
		return static::$_instance->has_server( $key );
	}
	
	/**
	 * get a FILE param
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public static function file( $key, $default = null ) 
	{
		return static::$_instance->file( $key, $default );
	}
	
	/**
	 * has a FILE param
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public static function has_file( $key ) 
	{
		return static::$_instance->has_file( $key );
	}

	
	/**
	 * get the client data
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public static function client( $key = null ) 
	{
		return static::$_instance->client( $key );
	}
	
	/**
	 * get the current requesting method
	 * GET, POST, PUT, DELETE
	 *
	 * @param string			$is
	 * @return string 
	 */
	public static function method( $is = null ) 
	{
		return static::$_instance->method( $is );
	}
	
	/**
	 * get the current protocol
	 *
	 * @return string 
	 */
	public static function protocol() 
	{
		return static::$_instance->protocol();
	}
	
	/**
	 * get the current host
	 *
	 * @return string 
	 */
	public static function host() 
	{
		return static::$_instance->host();
	}
	
	/**
	 * get the current server software
	 *
	 * @return string 
	 */
	public static function software() 
	{
		return static::$_instance->software();
	}
	
	/**
	 * get the http referrer SERVER:HTTP_REFERER
	 *
	 * @return string 
	 */
	public static function referrer() 
	{
		return static::$_instance->referrer();
	}
	
	/**
	 * get the requestet uri
	 *
	 * @param bool		$full	Don't cut the get params
	 * @return string 
	 */
	public static function uri( $full = false ) 
	{
		return static::$_instance->uri( $full );
	}
	
	/**
	 * get the requestet url
	 *
	 * @param bool		$full	Don't cut the get params
	 * @return string 
	 */
	public static function url() 
	{
		return static::$_instance->url();
	}
	
	/**
	 * is the current request an ajax request?
	 *
	 * @return bool
	 */
	public static function is_ajax() 
	{
		return static::$_instance->is_ajax();
	}
}