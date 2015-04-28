<?php namespace DB;
/**
 * DB Expression
 * This class is just an value holder so we are able to identify 
 * if a given string should not be escaped.
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Expression 
{
	/**
	 * The value holder 
	 *
	 * @var string
	 */
	public $value = null;
	
	/**
	 * The constructor that assigns our value
	 *
	 * @param string 		$value
	 * @return void
	 */
	public function __construct( $value )
	{
		$this->value = $value;
	}
	
	/**
	 * To string magic in case we want to echo the expression
	 *
	 * @return string
	 */
	public function __toString()
	{
		return $this->value;
	}
}