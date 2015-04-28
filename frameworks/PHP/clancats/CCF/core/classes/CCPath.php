<?php namespace Core;
/**
 * Path
 * This class helps finding files 
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCPath {
	
	/** 
	 * generate an path
	 * an path can conatin an namespace for example "core"
	 *
	 * if an path starts with an "/" only the suffix 
	 * will be added. 
	 *
	 * @param string		$path
	 * @param string		$prefix
	 * @param string		$sufix
	 */
	public static function get( $path, $prefix = '', $suffix = '' ) {
	
		if ( substr( $path, 0, 1 ) == '/' || substr( $path, 0, 3 ) == "::/" ) {
			return $path.$suffix;
		} 

		if ( strpos( $path, '::' ) === false ) {
			return \APPPATH.$prefix.$path.$suffix;
		}
		
		$name = explode( '::', $path );
		
		// do we have a namepsace
		if ( array_key_exists( $name[0], \CCFinder::$bundles ) ) 
		{
			return \CCFinder::$bundles[$name[0]].$prefix.$name[1].$suffix;
		}
		
		return false;
	}

	/**
	 * check if the internal path contains an namespace
	 *
	 * @param string 	$path
	 * @return bool
	 */
	public static function contains_namespace( $path ) {
		return strpos( $path, '::' ) !== false;
	}
	
	/**
	 * call static using the function name as prefix folder
	 * for example: CCPath::views( 'Core::myView', EXT )
	 *
	 * @param string		$name
	 * @param string		$args
	 */
	public static function __callStatic( $name, $args ) {
		
		// check if we have an path
		if ( !isset( $args[0] ) ) {
			return false;
		}
		
		// set the suffix default to an empty string
		if ( !isset( $args[1] ) ) {
			$args[1] = '';
		}
		
		// return our path
		return static::get( $args[0], $name.'/', $args[1] );
	}
}