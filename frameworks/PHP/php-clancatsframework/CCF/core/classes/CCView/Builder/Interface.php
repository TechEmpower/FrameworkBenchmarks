<?php namespace Core;
/**
 * CCView CCF template builder
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
interface CCView_Builder_Interface
{
	/**
	 * View builder contructor
	 *
	 * @param string 		$file
	 * @return void
	 */
	public function __construct( $file );
	
	/**
	 * Compile method returns the compiled php view file
	 *
	 * @return string
	 */
	public function compile();
}