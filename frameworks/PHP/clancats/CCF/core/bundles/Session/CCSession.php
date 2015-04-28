<?php namespace Session;
/**
 * Session handler
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCSession 
{
	/**
	 * Get an instance of a session manager
	 *
	 * @param string 			$manager
	 * @return Session\Manager
	 */
	public static function manager( $manager = null )
	{
		return Manager::create( $manager );
	}
	
	/**
	 * Get the fingerprint form an session
	 *
	 * @param string 			$manager
	 * @return string
	 */
	public static function fingerprint( $name = null )
	{
		return Manager::create( $name )->fingerprint();
	}
	
	/**
	 * Check if the given fingerprint or the default fingerprint
	 * parameter matches the curent session fingerprint.
	 *
	 * @param string 			$manager
	 * @return string
	 */
	public static function valid_fingerprint( $fingerprint = null, $name = null )
	{
		return Manager::create( $name )->valid_fingerprint( $fingerprint );
	}
	
	/**
	 * Get a value from the session
	 *
	 * @param string				$key
	 * @param string 			$default
	 * @param string				$manager
	 * @return Session\Manager
	 */
	public static function get( $key, $default, $manager = null )
	{
		return Manager::create( $manager )->get( $key, $default );
	}
	
	/**
	 * Get a value from the session and remove it afterwards
	 *
	 * @param string				$key
	 * @param string 			$default
	 * @param string				$manager
	 * @return Session\Manager
	 */
	public static function once( $key, $default, $manager = null )
	{
		return Manager::create( $manager )->once( $key, $default );
	}
	
	/**
	 * Set a value on the session
	 *
	 * @param string				$key
	 * @param string 			$value
	 * @param string				$manager
	 * @return Session\Manager
	 */
	public static function set( $key, $value, $manager = null )
	{
		return Manager::create( $manager )->set( $key, $value );
	}
	
	/**
	 * Similar to add but forces the element to be an array
	 * and appends an item.
	 *
	 * @param string				$key
	 * @param string 			$value
	 * @param string				$manager
	 * @return Session\Manager
	 */
	public static function add( $key, $value, $manager = null )
	{
		return Manager::create( $manager )->add( $key, $value );
	}
	
	/**
	 * Has a value on the session
	 *
	 * @param string				$key
	 * @param string				$manager
	 * @return Session\Manager
	 */
	public static function has( $key, $manager = null )
	{
		return Manager::create( $manager )->has( $key );
	}
	
	/**
	 * Delete a value on the session
	 *
	 * @param string				$key
	 * @param string				$manager
	 * @return Session\Manager
	 */
	public static function delete( $key, $manager = null )
	{
		return Manager::create( $manager )->delete( $key );
	}
}