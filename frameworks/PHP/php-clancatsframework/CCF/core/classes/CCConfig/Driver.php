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
interface CCConfig_Driver 
{	
	/**
	 * Read the configuration data 
	 * Get the configuration data and return them as array
	 *
	 * @param string		$file
	 * @return array
	 */
	public function read( $file );
	
	/**
	 * Write the configuration data
	 *
	 * @param string		$file
	 * @return void
	 */
	public function write( $file, $data );
	
	/**
	 * delete the configuration data
	 *
	 * @param string		$file
	 * @return void
	 */
	public function delete( $file );
}