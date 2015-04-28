<?php namespace CCUnit;
/**
 * CCUnit Ship
 *
 * @package       CCUnit
 * @author        Mario Döring <mario@clancats.com>
 * @version       1.0.0
 * @copyright     2010 - 2014 ClanCats GmbH
 */
class Model_Person extends \CCModel
{
	/*
	 * Defaults
	 */
	protected static $_defaults = array(
		'name'			=> '',
		'age'			=> 0,
		'library_id'	
	);
	
	/**
	 * Test modifier
	 */
	public function _get_modifier_line()
	{
		return $this->name.' '.$this->age;
	}
	
	/**
	 * Test modifier
	 */
	public function _get_modifier_age( $age )
	{
		if ( $age < 18 )
		{
			$age = 18;
		}
		
		return $age;
	}
	
	/**
	 * Test set modifier
	 */
	public function _set_modifier_age( $age )
	{
		if ( $age < 18 )
		{
			$age = 18;
		}
		
		return $age;
	}
}
