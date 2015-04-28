<?php namespace DB;
/**
 * Database mirgrations
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Migrator 
{
	/**
	 * The migration conifig file
	 *
	 * @var CCConfig
	 */
	protected static $config = null;
	
	/**
	 * Init the migrator directory
	 */
	public static function _init()
	{
		\ClanCats::directories( array( 'migration' => 'database/' ) );
		
		// read the migration configuration
		static::$config = \CCConfig::create( 'migrator', 'json' );
	}
	
	/**
	 * Get a migration path by name
	 *
	 * @param string 		$name
	 * @return string
	 */
	public static function path( $name, $time = null )
	{
		if ( is_null( $time ) )
		{
			$time = time();
		}
		
		$namespace = null;
		
		if ( strpos( $name, '::' ) !== false )
		{
			list( $namespace, $name ) = explode( '::', $name );
		}
		
		$name = explode( '/', $name );
		
		foreach( $name as $key => $value )
		{
			$name[$key] = \CCStr::clean_url( $value, '_' );
		}
		
		$name = implode( '/', $name );
		
		return \CCPath::get( ( $namespace ? $namespace.'::' : '' ) .$name.'_'.$time, \ClanCats::directory( 'migration' ), '.sql' );
	}
	
	/**
	 * Run all new migration
	 *
	 * @param bool			$silent 		In the silent mode the migrator ignores the migration file
	 * 
	 * @return void
	 */
	public static function migrate( $silent = false )
	{
		$migrations = $silent ? static::available() : static::unstaged();
		
		foreach( $migrations as $key => $value )
		{
			if ( empty( $value ) )
			{
				continue;
			}
			
			if ( \ClanCats::is_cli() )
			{
				\CCCli::info( 'found new "'.$key.'" migrations.' );
			}
			
			foreach( $value as $time => $path )
			{
				$migration = new static( $path );
				
				// run the migration
				$migration->up();
				
				if ( \ClanCats::is_cli() )
				{
					\CCCli::success( 'migrated '.$migration->name() );
				}
			}
			
			static::$config->set( $key.'.revision', $time );
		}
		
		if ( !$silent )
		{
			static::$config->write();
		}
	}
	
	/**
	 * Revert the last migration
	 *
	 * @return void
	 */
	public static function rollback()
	{
		// first of all we have to filter only the already migrated versions
		$available = static::available();
		
		foreach( $available as $key => $migrations )
		{
			foreach( $migrations as $time => $migration )
			{
				if ( $time > static::$config->get( $key.'.revision', 0 ) )
				{
					unset( $available[$key][$time] );
				}
			}
		}
		
		$revisions = array();
		
		foreach( $available as $key => $value )
		{
			if ( empty( $value ) )
			{
				continue;
			}
			
			foreach( $value as $name => $path )
			{
				$revisions[$name.'::'.$key] = $path;
			}
		}
		
		// nothing to rollback?
		if ( empty( $revisions ) )
		{
			if ( \ClanCats::is_cli() )
			{
				\CCCli::warning( 'nothing to rollback to.' );
			}
			
			return false;
		}
		
		ksort( $revisions );
		
		end( $revisions ); 
		list( $time, $key ) = explode( '::', key( $revisions ) );
		
		$migration = new static( array_pop( $revisions ) );
		
		// rollback the migration
		$migration->down();
		
		// get the lastet migration from the group
		$others = \CCArr::get( $key, $available );
		
		ksort( $others );
		array_pop( $others );
		end( $others );
		
		// update the config
		static::$config->set( $key.'.revision', key( $others ) );
		static::$config->write();
		
		return true;
	}
	
	/**
	 * The hard reset method deletes all tables from the database
	 *
	 * @param string 		$databse
	 */
	public static function hard_reset( $database = null )
	{
		$tables = DB::fetch( 'SHOW TABLES', array(), $database, array( 'assoc' ) );
		
		foreach( $tables as $table )
		{
			DB::run( 'DROP TABLE IF EXISTS '.reset( $table ), array(), $database );
		}
	}
	
	/**
	 * Returns the available migrations
	 *
	 * @return array
	 */
	public static function available()
	{
		$bundles = array_merge( \CCFinder::$bundles, array( 
			'app' => \CCPath::get( '', null ) 
		));
		
		$available = array();
		
		foreach( $bundles as $name => $path )
		{
			$directory = $path.\ClanCats::directory( 'migration' );
			
			if ( is_dir( $directory ) )
			{
				$available[strtolower($name)] = static::get_migrations( $directory );
			}
		}
		
		return $available;
	}
	
	/**
	 * Returns the available migrations
	 *
	 * @param string 		$path
	 * @return array
	 */
	public static function get_migrations( $path )
	{
		$objects = new \RecursiveIteratorIterator( new \RecursiveDirectoryIterator( $path ), \RecursiveIteratorIterator::SELF_FIRST );
		
		$files = array();
		
		foreach( $objects as $name => $object )
		{
			if ( \CCStr::extension( $name ) == 'sql' )
			{
				$files[\CCStr::cut( substr( basename( $name ), strrpos( basename( $name ),  '_' )+1 ), '.' )] = $name;
			}
		}
		
		ksort( $files );
		
		return $files;
	}
	
	/**
	 * Returns the unstaged migrations based on the configuration
	 *
	 * @return array
	 */
	public static function unstaged()
	{
		$available = static::available();
		
		foreach( $available as $key => $migrations )
		{
			foreach( $migrations as $time => $migration )
			{
				if ( $time <= static::$config->get( $key.'.revision', 0 ) )
				{
					unset( $available[$key][$time] );
				}
			}
		}
		
		return $available;
	}
	
	/**
	 * The migration sql file
	 *
	 * @var string
	 */
	protected $path = null;
	
	/**
	 * The migration name
	 *
	 * @var string
	 */
	protected $name = null;
	
	/**
	 * Creates a new migrator instance 
	 *
	 * @param string 		$path 	The path of the migration
	 * @return void
	 */
	protected function __construct( $path )
	{
		$this->path = $path;
		$this->name = \CCStr::cut( substr( basename( $path ), 0, strrpos( basename( $path ), '_' ) ), '.' );	
	}
	
	/**
	 * Returns the name of the migration
	 *
	 * @return string
	 */
	public function name()
	{
		return $this->name;
	}
	
	/**
	 * Migrates the current migration up
	 *
	 * @return void
	 */
	public function up()
	{
		\CCCli::info( $this->name.'...', 'migrating' );
		$this->run_queries( 'up' );
	}
	
	/**
	 * Migrates the current migration down
	 *
	 * @return void
 	 */
	public function down()
	{
		\CCCli::info( $this->name.'...', 'reverting' );
		$this->run_queries( 'down' );
	}
	
	/**
	 * Run all queries of specified group
	 *
	 * @param string 		$group
	 * @return void
	 */
	private function run_queries( $group )
	{
		if ( !$handle = fopen( $this->path , "r") )
		{
			throw new Exception( "Could not read migration: ".$this->path );
		}
		
		$mode = null;
		$query  = '';
		
		while ( !feof( $handle ) ) 
		{
			$buffer = trim( fgets( $handle, 4096 ) );
			
			$this->parse_mode( $mode, $buffer );
			
			if ( $this->is_comment( $buffer ) )
			{
				continue;
			}
			
			// only continue if we are in up mode
			if ( $mode === $group )
			{
				$query .= $buffer;
				
				if ( substr( rtrim( $query ), -1 ) == ';' ) 
				{
					// run the query
					DB::run( $query );
					
					// reset the query for the next one.
					$query = '';
				}
			}
			
		}
		
		fclose($handle);
	}
	
	/**
	 * Try to parse the current mode
	 *
	 * @param string 		$mode
	 * @param string			$buffer
	 * @return bool
	 */
	private function parse_mode( &$mode, $buffer )
	{
		if ( substr( trim( $buffer ), 0, 6 ) === '# --->' )
		{
			$mode = trim( substr( $buffer, 6 ) );
		}
	}
	
	/**
	 * Is this string an sql comment?
	 *
	 * @param string			$buffer
	 * @return bool
	 */
	private function is_comment( $buffer )
	{
		if ( is_string( $buffer ) && strlen( $buffer ) >= 1 )
		{
			if ( $buffer[0] == '#' )
			{
				return true;
			}
			
			if ( substr( $buffer, 0, 2 ) == '--' )
			{
				return true;
			}
		}
		else
		{
			return true;
		}
		
		return false;
	}
}