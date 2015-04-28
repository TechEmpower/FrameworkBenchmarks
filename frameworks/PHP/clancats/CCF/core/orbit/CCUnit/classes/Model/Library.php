<?php namespace CCUnit;
/**
 * CCUnit Ship
 *
 * @package       CCUnit
 * @author        Mario Döring <mario@clancats.com>
 * @version       1.0.0
 * @copyright     2010 - 2014 ClanCats GmbH
 */
class Model_Library extends \DB\Model
{	
	/*
	 * Current Table 
	 */
	protected static $_table = "libraries";

	/*
	 * Defaults
	 */
	protected static $_defaults = array(
		'id'	,
		'name'			=> '',
	);
	 
	public function person()
	{
		return $this->has_one( __NAMESPACE__."\\Model_DBPerson" );
	}
	
	public function books()
	{
		return $this->has_many( __NAMESPACE__."\\Model_Book" );
	}
}
