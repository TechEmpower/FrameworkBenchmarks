<?php namespace Core;
/**
 * ClanCats Shipyard Builder interface
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
interface CCShipyard_Interface
{		
	/**
	 * Get the current builder output
	 *
	 * @return string
	 */
	public function output();
	
	/**
	 * Write the file down
	 *
	 * There is no path parameter because the builder should no the 
	 * path based on other data like the name or namespace.
	 *
	 * @return bool
	 */
	public function write();
}