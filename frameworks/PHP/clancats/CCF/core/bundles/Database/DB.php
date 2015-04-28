<?php namespace DB;
/**
 * Database interface
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class DB 
{
	/**
	 * Connect to a database and return the connection status
	 * 
 	 * @param string 	$handler
 	 * @param bool
	 */
	public static function connect( $handler = null ) 
	{
		return Handler::create( $handler )->connected();
	}

	/**
	 * Get a database handler
	 * 
	 * @param string 	$handler
	 * @param bool
	 */
	public static function handler( $handler = null, $conf = null ) 
	{
		return Handler::create( $handler, $conf );
	}
	
	/**
	 * Create an Database expression object wich will not be escaped
	 *
	 * @param mixed 		$value
	 * @return Expression
	 */
	public static function raw( $value )
	{
		return new Expression( $value );
	}
	
	/**
	 * check if we have an db espression object
	 *
	 * @param string|Expession
	 * @return bool
	 */
	public static function is_expression( $param )
	{
		return $param instanceof Expression;
	}

	/**
	 * Return the logged queries
	 * 
	 * @param array
	 */
	public static function query_log() 
	{
		return Handler::log();
	}
	
	/**
	 * Returns the last executed query
	 * 
	 * @param string
	 */
	public static function last_query() 
	{
		return Handler::last_query();
	}

	/**
	 * Fetch data from an sql query
	 * Returns always an array of results
	 *
	 * @param string			$query
	 * @param array 			$params
	 * @return array
	 */
	public static function fetch( $query, $params = array(), $handler = null, $arguments = array( 'obj' ) ) 
	{
		return Handler::create( $handler )->fetch( $query, $params, $arguments );
	}

	/**
	 * Run an sql statement will return the number of affected rows
	 *
	 * @param string			$query
	 * @param array 			$params
	 * @return mixed
	 */
	public static function run( $query, $params = array(), $handler = null ) 
	{
		return Handler::create( $handler )->run( $query, $params );
	}
	
	/**
	 * Select data from the database
	 *
	 * @param string		$table
	 * @param array		$fields
	 * @return mixed
	 */
	public static function select( $table, $fields = array(), $handler = null ) 
	{
		return Query::select( $table, $fields, $handler );
	}
	
	/**
	 * Create a select query with an model assignment
	 *
	 * @param string		$table
	 * @param array		$fields
	 * @return mixed
	 */
	public static function model( $model, $handler = null ) 
	{
		$model_data = call_user_func( $model.'::_model' );
		return Query::select( $model_data['table'], null, $model_data['handler'] )
			->fetch_handler( $model.'::_fetch_handler' );
	}

	/**
	 * Find something, means select one record by key
	 *
	 * @param string 		$table
	 * @param int			$id
	 * @param string			$key
	 * @param string			$handler
	 * @return mixed
	 */
	public static function find( $table, $id, $key = null, $handler = null ) 
	{
		return Query::select( $table )->find( $id, $key, $handler );
	}

	/**
	 * Get the first result by key
	 *
	 * @param string 		$table
	 * @param string			$key
	 * @param string			$handler
	 * @return mixed
	 */
	public static function first( $table, $key = null, $handler = null ) 
	{
		return Query::select( $table )->first( $key, $handler );
	}

	/**
	 * Get the last result by key
	 *
	 * @param string 		$table
	 * @param string			$key
	 * @param string			$handler
	 * @return mixed
	 */
	public static function last( $table, $key = null, $handler = null ) 
	{
		return Query::select( $table )->last( $key, $handler );
	}

	/**
	 * Just return the count result 
	 *
	 * @param string 		$table
	 * @param string 		$handler
	 * @return int
	 */
	public static function count( $table, $handler = null ) 
	{
		return Query::select( $table )->count( $handler );
	}
	
	/**
	 * Create an insert query object
	 *
	 * @param string		$table
	 * @param array		$data
	 * @return mixed
	 */
	public static function insert( $table, $values = array(), $handler = null ) 
	{
		return Query::insert( $table, $values, $handler );
	}

	/**
	 * Create an update
	 *
	 * @param string			$table
	 * @param array 			$values
	 * @param string 		$handler
	 * @return DB\Query
	 */
	public static function update( $table, $data = null, $handler = null ) 
	{
		return Query::update( $table, $data, $handler );
	}

	/**
	 * Create delete query
	 *
	 * @param string			$table
	 * @param string 		$handler
	 * @return mixed
	 */
	public static function delete( $table, $handler = null ) 
	{
		return Query::delete( $table, $handler );
	}
	
	/**
	 * Attention with this one
	 *
	 * @param string			$table
	 * @param array 			$values
	 * @param string 		$handler
	 * @return DB\Query
	 */
	public static function truncate( $table )
	{
		return DB::run( 'truncate table `'.$table.';' );
	}
}
