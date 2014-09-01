<?php namespace Core;
/**
 * Configuration Driver
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCConfig_Array implements CCConfig_Driver 
{	
	/**
	 * Data holder
	 */
	protected static $data = array();
	
	/**
	 * Read the configuration data 
	 * Get the configuration data and return them as array
	 *
	 * @param string		$name
	 * @return array
	 */
	public function read( $name )
	{
		if ( array_key_exists( $name, static::$data ) )
		{
			return static::$data[$name];
		}
		return array();
	}
	
	/**
	 * Write the configuration data
	 *
	 * @param string		$file
	 * @return void
	 */
	public function write( $name, $data )
	{
		static::$data[$name] = $data;
	}
	
	/**
	 * delete the configuration data
	 *
	 * @param string		$file
	 * @return void
	 */
	public function delete( $name )
	{
		if ( array_key_exists( $name, static::$data ) )
		{
			unset( static::$data[$name] );
		}
	}
}