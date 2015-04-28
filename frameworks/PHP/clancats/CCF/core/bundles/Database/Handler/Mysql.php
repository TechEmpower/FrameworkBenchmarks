<?php namespace DB;
/**
 * Handler wraps PDO and handles the final queries
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Handler_Mysql extends Handler_Driver
{
	/**
	 * the string used for the PDO connection
	 *
	 * @var string
	 */
	protected $connection_string = 'mysql:host={host};dbname={db}';
	
	/**
	 * return the connection attributes
	 *
	 * @param array        $conf
	 * @return array
	 */
	protected function connection_attributes( $conf ) 
	{
	    return array(
	        \PDO::MYSQL_ATTR_INIT_COMMAND => "SET NAMES ".$conf['charset']
	    );
	}
}