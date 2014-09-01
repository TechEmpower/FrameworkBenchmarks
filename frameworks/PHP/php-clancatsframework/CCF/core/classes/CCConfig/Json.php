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
class CCConfig_Json extends CCConfig_File 
{
	/**
	 * The file extesion used for the
	 */
	const EXT = ".json";
	
	/**
	 * Read the file and return the data
	 *
	 * @return array
	 */
	protected function read_file( $file )
	{
		return CCJson::read( $file );
	}
	
	/**
	 * Write the file down to disk
	 *
	 * @return array
	 */
	protected function write_file( $file, $data )
	{
		CCJson::write( $file, $data, true );
	}
}