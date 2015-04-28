<?php namespace DB;
/**
 * The Query object
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Query
{	
	/**
	 * The default primary key
	 *
	 * @var string
	 */
	protected static $_default_key = null;
	
	/**
	 * Static init
	 */
	public static function _init()
	{
		static::$_default_key = \ClanCats::$config->get( 'database.default_primary_key', 'id' );
	}
	
	/**
	 * create a new query object with the default params
	 *
	 * @param string 		$table
	 * @param string 		$handler
	 *
	 * @return Query
	 */
	protected static function create( $table, $handler = null )
	{
		return new static( $table, $handler );
	}
	
	/**
	 * Create an insert
	 *
	 * @param string			$table
	 * @param array 			$values
	 * @param string 		$handler
	 * @return DB\Query
	 */
	public static function insert( $table, $values = array(), $handler = null )
	{
		return Query_Insert::create( $table, $handler )->values( $values );
	}
	
	/**
	 * Create an update
	 *
	 * @param string			$table
	 * @param array 			$values
	 * @param string 		$handler
	 * @return DB\Query
	 */
	public static function update( $table, $values = array(), $handler = null )
	{
		return Query_Update::create( $table, $handler )->set( $values );
	}
	
	/**
	 * Create a delete
	 *
	 * @param string			$table
	 * @param string 		$handler
	 * @return DB\Query
	 */
	public static function delete( $table, $handler = null )
	{
		return Query_Delete::create( $table, $handler );
	}
	
	/**
	 * Select data from the database
	 *
	 * @param string			$table
	 * @param array			$fields
	 * @param string	 		$handler
	 * @return mixed
	 */
	public static function select( $table, $fields = array(), $handler = null )
	{
		return Query_Select::create( $table, $handler )->fields( $fields );
	}
	
	/**
	 * The used table
	 * 
	 * @var string
	 */
	public $table = null;
	
	/**
	 * The handler instance to execute the query
	 * 
	 * @var Handler
	 */
	public $handler = null;
	
	/**
	 * The query where statements
	 *
	 * @var array
	 */
	public $wheres = array();
	
	/**
	 * the query offset
	 *
	 * @var int
	 */
	public $offset = 0;
	
	/**
	 * the query limit
	 *
	 * @var int
	 */
	public $limit = null;
	
	/**
	 * Query constructor
	 *
	 * @param string			$table
	 * @param string 		$handler
	 * @return void
	 */
	public function __construct( $table = null, $handler = null )
	{
		$this->table = $table;
		$this->handler( $handler );
	}
	
	/**
	 * Set the query table
	 *
	 * @param string 		$table
	 * @return void
	 */
	public function table( $table )
	{
		$this->table = $table;
	}
	
	/**
	 * Set the query handler
	 *
	 * @param string 		$handler		The DB handler instance name
	 * @return void
	 */
	public function handler( $handler )
	{
		$this->handler = Handler::create( $handler );
	}
	
	/**
	 * Create a where statement
	 *
	 * where query: <$column> <$param1> <$param2> <$type>
	 * 
	 * example:
	 *     ->where( 'name', 'ladina' ) // name = landina
	 *     ->where( 'age', '>', 18 )
	 *     ->where( 'name', 'in', array( 'johanna', 'jennifer' ) )
	 *
	 * @param string		$column			The SQL column
	 * @param mixed		$param1			
	 * @param mixed		$param2
	 * @param string		$type			The where type ( and, or )
	 *
	 * @return self
	 */
	public function where( $column, $param1 = null, $param2 = null, $type = 'and' ) 
	{
		// if this is the first where element we are going to change
		// the where type to 'where'
		if ( empty( $this->wheres ) )
		{
			$type = 'where';
		}
		
		// when column is an array we assume to make a bulk and where.
		// array( 'name' => 'ladina', 'language' => 'de' )
		// where name = ladina and language = de
		if ( is_array( $column ) ) 
		{
			foreach( $column as $key => $val ) 
			{
				$this->where( $key, $val, null, $type );	
			}
			
			return $this;
		}
		
		// to make nested wheres possible you can pass an closure 
		// wich will create a new query where you can add your nested wheres
		if ( is_closure( $column ) )
		{
			$this->wheres[] = array( $type, $column ); return $this;
		}
		
		// when param2 is null we replace param2 with param one as the 
		// value holder and make param1 to the = operator.
		if ( is_null( $param2 ) ) 
		{
			$param2 = $param1; $param1 = '=';
		}
		
		// if the param2 is an array we filter it. Im no more sure why 
		// but it's there since 2 years so i think i had a reason.
		if ( is_array( $param2 ) ) 
		{
			$param2 = array_unique( $param2 );
		}
		
		$this->wheres[] = array( $type, $column, $param1, $param2 ); return $this;
	}
	
	/**
	 * Create an or where statement
	 *
	 * This is the same as the normal where just with a fixed type
	 *
	 * @param string		$column			The SQL column
	 * @param mixed		$param1			
	 * @param mixed		$param2
	 *
	 * @return self
	 */
	public function or_where( $column, $param1 = null, $param2 = null )
	{
		return $this->where( $column, $param1, $param2, 'or' );
	}
	
	/**
	 * Create an and where statement
	 *
	 * This is the same as the normal where just with a fixed type
	 *
	 * @param string		$column			The SQL column
	 * @param mixed		$param1			
	 * @param mixed		$param2
	 *
	 * @return self
	 */
	public function and_where( $column, $param1 = null, $param2 = null )
	{
		return $this->where( $column, $param1, $param2, 'and' );
	}
	
	/**
	 * Set the query limit
	 *
 	 * @param int		$limit
	 * @param int 		$limit2
	 * @return void
	 */
	public function limit( $limit, $limit2 = null ) 
	{
		if ( !is_null( $limit2 ) ) 
		{
			$this->offset = (int) $limit;
			$this->limit  = (int) $limit2;
		}
		else 
		{
			$this->limit = $limit;
		}
		
		return $this;
	}
	
	/**
	 * Create an query limit based on a page and a page size
	 *
	 * @param int		$page
	 * @param int 		$size
	 * @return void
	 */
	public function page( $page, $size = 25 ) 
	{	
		// validate page
		if ( (int) $page < 1 )
		{
			$page = 1;
		}
		
		$this->limit = (int) $size;
		$this->offset = (int) ( ( $size * $page ) - $size );
		
		return $this;
	}
	
	/**
	 * Build the query to a string
	 *
	 * @return string
	 */
	public function build()
	{
		$this->handler->builder()->clear_parameters();
	}
	
	/**
	 * Build and execute the current query
	 * This wil run the local build function and pass the parameters
	 * from the builder object in the handler
	 *
	 * @param string 		$handler
	 * @return mixed
	 */
	public function run( $handler = null )
	{
		if ( !is_null( $handler ) )
		{
			$this->handler( $handler );
		}
		
		return $this->handler->run( $this->build(), $this->handler->builder()->parameters );
	}
}