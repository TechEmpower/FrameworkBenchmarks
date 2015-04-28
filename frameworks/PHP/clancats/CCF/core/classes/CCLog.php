<?php namespace Core;
/**
 * Logger
 * This class logs errors, infos ect. to a file.
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCLog 
{
	/**
	 * log data holder
	 *
	 * @var array
	 */
	protected static $_data = array();
	
	/**
	 * static initialisation
	 *
	 * @return void
	 */
	public static function _init() 
	{
		CCEvent::after( 'CCF.shutdown', array( get_class(), 'write' ) );
	}
	
	/**
	 * add something to our log
	 *
	 * @param string		$message
	 * @param string		$key
	 * @return void
	 */
	public static function add( $message, $key = 'info' ) 
	{
		static::$_data[] = "  [{$key}] ".$message;
	}
	
	/**
	 * add an error to the log
	 *
	 * @param string		$message
	 * @return void
	 */
	public static function error( $message ) 
	{
		static::add( $message, 'error' );
	}
	
	/**
	 * add an warning to the log
	 *
	 * @param string		$message
	 * @return void
	 */
	public static function warning( $message ) 
	{
		static::add( $message, 'warning' );
	}
	
	/**
	 * clear the log
	 *
	 * @return void
	 */
	public static function clear() 
	{
		static::$_data = array();
	}
	
	/**
	 * write the log down to disk
	 *
	 * @return void
	 */
	public static function write() 
	{
		if ( empty( static::$_data ) )
		{
			return;
		}
		
		$buffer = date("H:i:s")." - ".CCServer::method().' '.CCServer::server('REQUEST_URI')."\n";
		$buffer .= implode( "\n", static::$_data );
		
		CCFile::append( CCStorage::path( 'logs/'.date( 'Y-m' ).'/'.date( 'd' ).'.log' ), $buffer."\n" );
		
		// clear
		static::clear();
	}
}