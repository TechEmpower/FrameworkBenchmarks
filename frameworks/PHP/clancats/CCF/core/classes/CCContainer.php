<?php namespace Core;
/**
 * Container
 * the container can store callbacks based on an key
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCContainer
{
	/*
	 * the holder
	 */
	private static $_container = array();

	/**
	 * register an container 
	 *
	 * @param string 	$key		
	 * @param mixed		$callback
	 * @return void
	 */
	public static function mind( $key, $callback )
	{
		static::$_container[$key] = $callback;
	}

	/**
	 * does this container exist?
	 *
	 * @param string 	$key		
	 * @return void
	 */
	public static function has( $key )
	{
		return array_key_exists( $key, static::$_container );
	}

	/**
	 * Is this callable
	 *
	 * @param string 	$key	
	 * @return bool
	 */
	public static function is_callable( $key )
	{
		if ( is_callable( $key ) )
		{
			return true;
		}

		if ( array_key_exists( $key, static::$_container ) )
		{
			return true;
		}

		return false;
	}

	/**
	 * call a container 
	 *
	 * @param string 	$key		
	 * @param mixed		$callback
	 * @return void
	 */
	public static function call()
	{
		$arguments = func_get_args();

		// get the key
		$key = array_shift( $arguments );

		// container call
		if ( is_string( $key ) && array_key_exists( $key, static::$_container ) && is_callable( static::$_container[$key] ) )
		{
			return call_user_func_array( static::$_container[$key], $arguments );
		}

		if ( !is_callable( $key ) )
		{
			throw new CCException( "CCContainer::call - Cannot call '".$key."' invalid callback." );
		}

		// default callback
		return call_user_func_array( $key, $arguments );
	}
}