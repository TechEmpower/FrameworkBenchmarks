<?php namespace Core;
/**
 * Helper to manage and create files
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCStorage 
{
	/**
	 * Paths holder
	 *
	 * @var array
	 */
	protected static $paths = array();
	
	/**
	 * URls holder
	 *
	 * @var array
	 */
	protected static $urls = array();
	
	/**
	 * Path replacement params
	 *
	 * @var array
	 */
	protected static $params = array();

	/**
	 * Default holder name
	 *
	 * @var array
	 */
	protected static $default = 'main';

	/**
	 * Static init load the paths and urls from the configuration
	 *
	 * @return void
	 */
	public static function _init()
	{
		static::$paths = ClanCats::$config->get( 'storage.paths' );
		static::$urls = ClanCats::$config->get( 'storage.urls' );
	}
	
	/**
	 * Add a replacement param
	 *
	 * @param string 		$key
	 * @param string 		$value
	 *
	 * @return void
	 */
	public static function param( $key, $value )
	{
		static::$params[$key] = CCStr::clean_url( $value );
	}
	
	/**
	 * Adds a new storage directory 
	 *
	 * @param string 		$key
	 * @param string 		$path
	 * @param string 		$url
	 * 
	 * @return void
	 */
	public static function add( $key, $path, $url = null )
	{
		static::$paths[$key] = $path;
		
		if ( !is_null( $url ) )
		{
			static::$urls[$key] = $url;
		}
	}
	
	/**
	 * Prepares a file with the parameters
	 *
	 * @param string 		$file
	 * @return $file
	 */
	public static function file( $file )
	{
		$params = array_merge( static::$params, array(
			'time' => time(),
			'fingerprint' => \CCSession::fingerprint(),
			'random' => CCStr::random(),
		));
		
		foreach( $params as $param => $value )
		{
			$file = str_replace( ':'.$param, $value, $file );
		}
		
		return $file;
	}
	
	/**
	 * Get a storage path ( absolute )
	 *
	 * @param string		$file
	 * @param string		$key
	 * @return string
	 */
	public static function path( $file = null, $key = null ) 
	{
		// get the storage key
		if ( is_null( $key ) ) 
		{
			$key = static::$default;
		}

		// check if path exists
		if ( !isset( static::$paths[$key] ) ) 
		{
			throw new CCException( 'CCStorage - use of undefined storage path '.$key.'.' );
		}
			
		if ( strpos( $file, ':' ) )
		{
			$file = static::file( $file );
		}
		
		return static::$paths[$key].$file;
	}

	/**
	 * Get the public url to a file if available
	 *
	 * @param string		$file
	 * @param string		$key
	 * @return string
	 */
	public static function url( $file = null, $key = null ) 
	{
		// get the storage key
		if ( is_null( $key ) ) 
		{
			$key = static::$default;
		}

		// check if path exists
		if ( !isset( static::$urls[$key] ) ) 
		{
			throw new CCException( 'CCStorage - use of undefined public url '.$key.'.' );
		}

		return CCUrl::to( static::$urls[$key].$file );
	}

	/**
	 * Write a file to the storage
 	 *
	 * @param string		$file
	 * @param string		$key
	 * @return string
	 */
	public static function write( $file, $content, $key = null ) 
	{
		return CCFile::write( static::path( $file, $key ), $content );
	}

	/**
	 * Write a file to the storage
	 *
	 * @param string		$file
	 * @param string		$key
	 * @return string
	 */
	public static function touch( $file, $key = null ) 
	{
		return static::write( $file, '', $key );
	}
}