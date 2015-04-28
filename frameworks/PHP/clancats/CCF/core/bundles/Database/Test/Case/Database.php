<?php namespace DB;
/**
 * Database test case
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */

use CCConfig;
use CCPath;

class Test_Case_Database extends Test_Case
{
	/**
	 * people data bulk
	 */
	public static function people_provider_bulk() 
	{
		return array(
			array(
				array(
					array( 'name' => 'Mario', 'age' => 20, 'library_id' => 1 ),
					array( 'name' => 'Ladina', 'age' => 20, 'library_id' => 2 ),
					array( 'name' => 'Johanna', 'age' => 18, 'library_id' => 1 ),
					array( 'name' => 'Jenny', 'age' => 22, 'library_id' => 0 ),
					array( 'name' => 'Melanie', 'age' => 19, 'library_id' => 0 ),
					array( 'name' => 'Tarek', 'age' => 20, 'library_id' => 3 ),
					array( 'name' => 'John', 'age' => 42, 'library_id' => 4 ),
				),
			),
		);
	}
	
	/**
	 * people data
	 */
	public static function people_provider() 
	{
		return array(
			array(
				array( 'name' => 'Mario', 'age' => 20, 'library_id' => 1 ),
			),
			array(
				array( 'name' => 'Ladina', 'library_id' => 2, 'age' => 20 ),
			),
			array(
				array( 'name' => 'Johanna', 'age' => -18, 'library_id' => 1 ),
			),
			array(
				array( 'age' => 22, 'library_id' => 0,  'name' => 'Jenny' ),
			),
		);
	}
	
	/**
	 * Return a string of the database config name 
	 * used for the test in this class
	 *
	 * @return string
	 */
	protected static function main_database()
	{
		// these tests should use a seperate table
		return 'phpunit';
	}
}