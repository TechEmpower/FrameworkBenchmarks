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
class Handler
{
	/**
	 * Instance holder
	 *
	 * @var array
	 */
	protected static $_instances = array();

	/**
	 * Default database instance name
	 *
	 * @var string
	 */
	private static $_default = 'main';

	/**
	 * The query log if enabeled
	 *
	 * @var array
	 */
	private static $_query_log = array();

	/**
	 * Static init
	 * When we are in development then we append the qurey log to body
	 *
	 * @codeCoverageIgnore
	 *
	 * @return void
	 */
	public static function _init() 
	{	
		if ( \ClanCats::in_development() ) 
		{ 
			// add a hook to the main resposne
			\CCEvent::mind( 'response.output', function( $output ) {
				
				if ( strpos( $output, '</body>' ) === false ) 
				{
					return $output;
				}
				
				$table = \UI\Table::create( array(
					 'style'		=> array(
					 	'width'	=> '100%',
					 ),
					 'cellpadding' 	=> '5',
					 'class'			=> 'table debug-table debug-table-db',
				));
				
				$table->header( array( '#', 'query' ) );
				
				foreach( static::log() as $key => $item )
				{
					$table->row( array( 
						$key+1,
						$item
					));
				}
			
				return str_replace( '</body>', $table."\n</body>", $output );
			});
		}
	}

	/**
	 * Get a database handler instance
	 * The objects get stored on runtime so you can use this function 
	 * multipe times wihout reconecting to the database.
	 *
	 * @param string			$name
	 * @param array 			$conf	You can pass optionally a configuration directly. This will overwrite.
	 * @return DB_Handler
	 */
	public static function create( $name = null, $conf = null ) 
	{
		if ( is_null( $name ) ) 
		{
			$name = static::$_default;
		}
		
		if ( !is_null( $conf ) && is_array( $conf ) )
		{
			return static::$_instances[$name] = new static( $conf );
		}
		
		if ( !isset( static::$_instances[$name] ) )
		{
			static::$_instances[$name] = new static( $name );
		}
		
		return static::$_instances[$name];
	}
	
	/**
	 * Kill all database connections
	 *
	 * @return void
	 */
	public static function kill_all_connections()
	{
		static::$_instances = array();
	}

	/**
	 * Get the query log 
	 * 
	 * @return array
	 */
	public static function log()
	{
		return static::$_query_log;
	}
	
	/**
	 * Get the query log 
	 * 
	 * @return array
	 */
	public static function last_query()
	{
		return \CCArr::last( static::$_query_log );
	}
	
	/**
	 * The current connection
	 *
	 * @var DB\Handler_Driver
	 */
	protected $_driver;
	
	/**
	 * The current query builder
	 *
	 * @var DB\Builder
	 */
	protected $_builder;

	/**
	 * Are we connected to default Database
	 *
	 * @var bool
	 */
	protected $_connected = false;

	/** 
	 * Handler constructor and connect to the database
	 *
	 * @param string		$name
	 * @return void
	 */
	protected function __construct( $name ) 
	{
		$this->connect( $name );
	}
	
	/**
	 * Get the current driver
	 *
	 * @return DB\Handler_Driver
	 */
	public function driver()
	{
		return $this->_driver;
	}
	
	/**
	 * Set the current driver
	 *
	 * @param string		$driver		The full driver class ( DB\Handler_ExampleDriver )
	 * @return void
	 */
	private function set_driver( $driver )
	{
		$this->_driver = new $driver;
	}
	
	/**
	 * Get the current query builder
	 *
	 * @return DB\Builder
	 */
	public function builder()
	{
		return $this->_builder;
	}
	
	/**
	 * Set the current builder
	 *
	 * @param string		$driver		The full driver class ( DB\Builder_ExampleDriver )
	 * @return void
	 */
	private function set_builder( $driver )
	{
		$this->_builder = new $driver;
	}

	/**
	 * Try to etablish a connetion to the database. Also assign the connection 
	 * and query builder to the current DB driver.
	 *
	 * @param string|array 	$name	When passing an array it will be uesed as configuration.
	 * @return void
	 */
	protected function connect( $name ) 
	{
		if ( $this->_connected ) 
		{
			return true; 
		}
		
		// check if the name is an array. This way we can
		// pass the configuration directly. We need this 
		// to create for example an handler without having
		// the configuration in the database conf file.
		if ( is_array( $name ) )
		{
			$config = $name;
		}
		else 
		{
			$config = \CCConfig::create( 'database' )->get( $name );
			
			// check for an alias. If you set a string 
			// in your config file we use the config 
			// with the passed key.
			if ( is_string( $config ) ) 
			{
				$config = \CCConfig::create( 'database' )->get( $config );
			}
		}

		// Setup the driver class. We simply use name 
		// from the confif file and make the first letter 
		// capital. example: Handler_Mysql, Handler_Sqlite etc.
		$driver_class = __NAMESPACE__."\\Handler_".ucfirst( $config['driver'] );
		
		if ( !class_exists( $driver_class ) )
		{
			throw new Exception( "DB\\Handler::connect - The driver (".$driver_class.") is invalid." );
		}
		
		$this->set_driver( $driver_class );
		
		// setup the builder the same way as the handler.
		$driver_class = __NAMESPACE__."\\Builder_".ucfirst( $config['driver'] );
		
		if ( !class_exists( $driver_class ) )
		{
			throw new Exception( "DB\\Handler::connect - The builder (".$driver_class.") is invalid." );
		}
		
		$this->set_builder( $driver_class );
		
		// finally try to connect the driver with the databse
		if ( $this->driver()->connect( $config ) ) 
		{
			return $this->_connected = true;
		}

		return $this->_connected = false;
	}
	
	/**
	 * Get the connected status
	 *
	 * @return bool
	 */
	public function connected()
	{
		return $this->_connected;
	}
	
	/**
	 * Run the query and return the PDO statement
	 *
	 * @param string			$query
	 * @param array 			$params
	 * @return array
	 */
	public function statement( $query, $params = array() )
	{
		// we alway prepare the query even if we dont have parameters
		$sth = $this->driver()->connection()->prepare( $query );
		
		// because we set pdo the throw exception on db errors
		// we catch them here to throw our own exception.
		try 
		{
			$sth->execute( $params );
		}
		catch ( \PDOException $e ) 
		{
			throw new Exception( "DB\\Handler - PDOException: {$e->getMessage()} \n Query: {$query}" );
		}
		
		// In development we alway log the query into an array.
		if ( \ClanCats::in_development() ) 
		{	
			$keys = array();
			
			foreach ( $params as $key => $value ) 
			{
				if ( is_string( $key ) ) 
				{
					$keys[] = '/:'.$key.'/';
				} else {
					$keys[] = '/[?]/';
				}
			}
			
			static::$_query_log[] = preg_replace( $keys, $params, $query, 1 );
		}
		
		return $sth;
	}
	
	/**
	 * Run the query and fetch the results
	 *
	 * You can pass arguments on the third parameter.
	 * These parameter are just the normal PDO ones but in short.
	 *
	 *     obj = \PDO::FETCH_OBJ
	 *     assoc = \PDO::FETCH_ASSOC
	 *
	 * @param string			$query
	 * @param array 			$params
	 * @return array
	 */
	public function fetch( $query, $params = array(), $arguments = array( 'obj' ) )
	{
		$sth = $this->statement( $query, $params );
		
		$args = null;
		
		foreach( $arguments as $argument )
		{
			$args |= constant( "\PDO::FETCH_".strtoupper( $argument ) );
		}
				
		return $sth->fetchAll( $args );
	}
	
	/**
	 * Run the query and get the correct response
	 *
	 * INSERT -> last id
	 * UPDATE -> affected rows
	 * DELETE -> affected rows
	 * etc...
	 * 
	 * @param string			$query
	 * @param array 			$params
	 * @return array
	 */
	public function run( $query, $params = array() )
	{
		$sth = $this->statement( $query, $params );
		
		$type = strtolower( substr( $query, 0, strpos( $query, ' ' ) ) );
	
		switch ( $type ) 
		{
			case 'update':
			case 'delete':
				return $sth->rowCount();
			break;
			
			case 'insert':
				return $this->driver()->connection()->lastInsertId();
			break;
		}
		
		return $sth;
	}
}