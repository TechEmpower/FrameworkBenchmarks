<?php namespace CCUnit;
/**
 * CCUnit Ship
 *
 * @package       CCUnit
 * @author        Mario Döring <mario@clancats.com>
 * @version       1.0.0
 * @copyright     2010 - 2014 ClanCats GmbH
 */
class Model_DBPerson extends \DB\Model
{	
	/*
	 * Current Table 
	 */
	protected static $_table = "people";
	
	/*
	 * Defaults
	 */
	protected static $_defaults = array(
		'id'	,
		'name'			=> '',
		'age'			=> array( 'int', 0 ),
		'library_id'		=> array( 'int'  )
	);
	
	public function library()
	{
		return $this->belongs_to( __NAMESPACE__."\\Model_Library" );
	}
}
