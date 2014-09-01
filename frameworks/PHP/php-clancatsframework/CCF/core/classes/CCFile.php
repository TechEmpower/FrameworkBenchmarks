<?php namespace Core;
/**
 * File creater 
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCFile 
{		
	/**
	 * If true CCFile will print information about file operations
	 *
	 * @var bool
	 */
	protected static $_print_infos = false;

	/**
	 * Enable the print infos
	 *
	 * @return void
	 */
	public static function enable_infos()
	{
		static::$_print_infos = true;
	}

	/**
	 * Enable the print infos
	 *
	 * @return void
	 */
	public static function disable_infos()
	{
		static::$_print_infos = false;
	}

	/**
	 * Is CCFile able to print information
	 *
	 * @return bool
	 */
	protected static function _can_print()
	{
		return ( ClanCats::is_cli() && static::$_print_infos ) ? true : false;
	}

	/**
	 * get a file 
	 *
	 * @param string 	$path
	 * @return mixed 	returns false if the file could not be loaded.
	 */
	public static function read( $path ) 
	{
		if ( file_exists( $path ) ) 
		{
			return file_get_contents( $path );
		}
		return false;
	}

	/**
	 * Create a directory 
	 *
	 * @param string 		$path
	 * @return void
	 */
	public static function mkdir( $path )
	{
		if ( !is_dir( dirname( $path ) ) ) 
		{
			if ( !mkdir( dirname( $path ), 0755, true ) ) 
			{
				throw new CCException( "CCFile - could not create directory: ".dirname( $path ) );
			}
		}
	}

	/**
	 * save a file
	 * 
	 * @param string 	$path
	 * @param string		$content
	 * @return bool
	 */
	public static function write( $path, $content ) 
	{
		static::mkdir( $path );

		// if writing the file fails
		if ( file_put_contents( $path, $content, LOCK_EX ) === false ) 
		{

			if ( static::_can_print() ) 
			{
				CCCli::line( CCCli::color( 'failure', 'red' ).' creating '.$path );
			}

			return false;
		}

		// everything went good
		if ( static::_can_print() ) 
		{
			CCCli::line( CCCli::color( 'created', 'green' ).' '.$path );
		}

		return true;
	}

	/**
	 * append to a file
	 * 
	 * @param string 	$path
	 * @param string	$content
	 * @return bool
	 */
	public static function append( $path, $content ) 
	{
		if ( !is_dir( dirname( $path ) ) ) 
		{
			if ( !mkdir( dirname( $path ), 0755, true ) ) 
			{
				throw new CCException( "CCFile - could not create directory: ".dirname( $path ) );
			}
		}
		return file_put_contents( $path, $content, LOCK_EX | FILE_APPEND );
	}

	/**
	 * Delete a file
	 * This function is going to remove a file from your filesystem
	 *
	 * @param string 	$path
	 * @return bool
	 */
	public static function delete( $path ) 
	{
		$success = false;

		if ( file_exists( $path ) ) 
		{
			$success = unlink( $path );
		}

		if ( static::_can_print() ) 
		{
			if ( $success )
			{
				CCCli::line( CCCli::color( 'removed', 'green' ).' '.$path );
			}
			else 
			{
				CCCli::line( CCCli::color( 'removing failure', 'red' ).' '.$path );
			}
		}

		return $success;
	}

	/**
	 * Returns a simple list of files using glob
	 *
	 * @param string 		$path
	 * @return array
	 */
	public static function ls( $path )
	{
		return glob( $path );
	}

	/**
	 * get the path of an uplaoded file
	 *
	 * @param string	$key
	 * @return string|false
	 */
	public static function upload_path( $key ) 
	{	
		return CCArr::get( 'tmp_name', CCIn::file( $key, array( 'tmp_name' => false ) ) );
	}

	/**
	 * Move user uploads to another dir
	 *
	 * @param string	$key
	 * @param string 	$path
	 * @return bool
	 */
	public static function upload( $key, $path ) 
	{

		if ( $file = static::upload_path( $key ) ) {
			return move_uploaded_file( $file, $path );
		}

		return false;
	}
}