<?php namespace Core;
/**
 * Orbit
 * plugin / modules handler
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCOrbit {
	
	/**
	 * orbit data
	 */
	protected static $data = array();
	
	/*
	 * loaded ships holder
	 */
	protected static $ships = array();
	
	/**
	 * init the orbit data
	 */
	public static function _init() 
	{
		static::$data = CCConfig::create( ClanCats::$config->get( 'orbit.data' ), 'json' );
	}
	
	/**
	 * return data of an ship
	 */
	public static function ship( $ship ) 
	{
		if ( array_key_exists( $ship, static::$ships ) )
		{
			return static::$ships[$ship];
		}
		return false;
	}
	
	/**
	 * check if the give ship is loaded
	 *
	 * @param string 	$ship
	 * @return bool
	 */
	public static function has( $ship )
	{
		return array_key_exists( $ship, static::$ships );
	}
	
	/**
	 * return data of an ship by the given namespace
	 *
	 * @param string 	$namespace
	 * @return CCOrbit_Ship | false
	 */
	public static function ship_by_namespace( $namespace ) 
	{
		foreach( static::$ships as $ship )
		{
			if ( $ship->namespace == $namespace )
			{
				return $ship;
			}
		}
		return false;
	}
	
	/**
	 * return all flying ships
	 */
	public static function ships() 
	{
		return static::$ships;
	}
	
	/**
	 * return all installed ships
	 */
	public static function installed_ships() 
	{
		$ships = static::$data->get( 'installed', array() );
		
		foreach ( $ships as $key => $ship ) 
		{
			$ships[$key] = CCROOT.$ship;
		}
		
		return $ships;
	}
	
	/**
	 * return all installed ships
	 */
	public static function enter_installed_ships() 
	{
		// enter installed ship
		static::enter( array_values( static::installed_ships() ) );
	}
	
	/**
	 * install a ship
	 *
	 * string 	$path | the ship name / the folder. If the path is not absolute, ORBITPATH gets used
	 */
	public static function install( $path ) 
	{
		
		// load ship at path
		$ship = CCOrbit_Ship::create( $path );
		
		if ( static::$data->has( 'installed.'.$ship->name ) ) {
			throw new CCException( "CCOrbit::install - {$ship->name} ship already installed." );
		}
		
		if ( $ship->install !== false ) {
			$ship->event( $ship->install );
		}
		
		static::$data->set( 'installed.'.$ship->name, CCStr::strip( $ship->path, CCROOT ) );
		
		// save changes
		static::$data->write( 'json' );
	}
	
	/**
	 * install a ship
	 *
	 * string 	$path | the ship name / the folder. If the path is not absolute, ORBITPATH gets used
	 */
	public static function uninstall( $path ) 
	{	
		// load ship at path
		$ship = CCOrbit_Ship::create( $path );
		
		if ( !static::$data->has( 'installed.'.$ship->name ) ) {
			throw new CCException( "CCOrbit::uninstall - {$ship->name} ship is not installed." );
		}
		
		if ( $ship->uninstall !== false ) {
			$ship->event( $ship->uninstall );
		}
		
		static::$data->delete( 'installed.'.$ship->name );
		
		// save changes
		static::$data->write( 'json' );
	}
	
	/**
	 * Add a ship
	 * this loads the ship loader file
	 *
	 * @param string		$path
	 * @return bool
	 */
	public static function enter( $path ) 
	{	
		if ( !is_array( $path ) ) 
		{
			$path = array( $path );
		}
		
		foreach( $path as $ship ) 
		{	
			// load ship at path
			$ship = CCOrbit_Ship::create( $ship );
			
			if ( array_key_exists( $ship->name, static::$ships ) ) 
			{
				throw new CCException( "CCOrbit::enter - {$ship->name} ship already entered." );
			}
			
			if ( $ship->wake !== false ) {
				$ship->event( $ship->wake );
			}
			
			CCProfiler::check( "CCOrbit - ship {$ship->name} launched." );
			
			// add to our loaded ships
			static::$ships[$ship->name] = $ship;
		}
	}
}