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

class Test_Case extends \PHPUnit_Framework_TestCase
{
	/**
	 * Is a database configured?
	 *
	 * @var bool
	 */
	public static $dbtests = false;
	
	/**
	 * Return a string of the database config name 
	 * used for the test in this class
	 *
	 * @return string
	 */
	protected static function main_database()
	{
		// the normal test case uses the app database
		return 'app';
	}

	/**
	 * Check if DB test are possible
	 *
	 * @return void
	 */
	public static function setUpBeforeClass() 
	{	
		$current_datatbase = static::main_database();
		
		// lets make sure that we have an db configuration for phpunit
		if ( CCConfig::create( 'database' )->has( $current_datatbase ) )
		{	
			// lets try to connect to that database if the conection
			// fails we just return and continue the other tests
			try { DB::connect( $current_datatbase ); }
			catch ( \PDOException $e ) { return; }

			// connection succeeded?
			static::$dbtests = true;

			// overwrite the main db
			CCConfig::create( 'database' )->set( 'main', $current_datatbase );

			// kill all connections
			Handler::kill_all_connections();
			
			// check for any sql files that should be executed needed 
			// for theses tests. We simply check if a file exists in the
			// CCUnit bundle "db_<current_database>.sql"
			if ( file_exists( CCPath::get( 'CCUnit::db_'.$current_datatbase.'.sql' ) ) )
			{
				$queries = explode( ';', file_get_contents( CCPath::get( 'CCUnit::db_'.$current_datatbase.'.sql' ) ) );
				
				foreach( $queries as $query )
				{
					$query = trim( $query );
				
					if ( !empty( $query ) )
					{
						DB::run( $query, array(), 'phpunit' );
					}
				}
			}
		}
	}

	public static function tearDownAfterClass() 
	{	
		// write the main database back to app
		CCConfig::create( 'database' )->set( 'main', 'app' );
		
		// kill all connections
		Handler::kill_all_connections();
	}

	/**
	 * Check if we can execute db tests
	 * And add a warning to phpunit that we skipped the test.
	 *
	 * @return void
	 */
	protected function setUp()
	{
		if ( !static::$dbtests )
		{
			$this->markTestSkipped( "Warning! Could not connect to phpunit DB. skipping DB unit test." );
		}
	}
}