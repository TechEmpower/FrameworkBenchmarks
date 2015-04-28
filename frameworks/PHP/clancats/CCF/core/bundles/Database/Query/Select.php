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
class Query_Select extends Query
{
	/**
	 * fields container
	 *
	 * @var array 
	 */
	public $fields = array();

	/**
 	 * make an distinct
	 *
	 * @var bool 
	 */
	public $distinct = false;
	
	/**
	 * order by container
	 *
	 * @var array 
	 */
	public $orders = array();
	
	/**
	 * order by container
	 *
	 * @var array 
	 */
	public $groups = array();

	/**
	 * group the results
	 *
	 * @var false|string 
	 */
	public $group_result = false;
	
	/**
	 * forward key
	 *
	 * @var false|string 
	 */
	public $forward_key = false;
	
	/**
	 * the fetching arguments
	 *
	 * @var array
	 */
	public $fetch_arguments = array( 'obj' );
	
	/**
	 * the fetching handler
	 *
	 * @var callback
	 */
	public $fetch_handler = null;

	/**
	 * Distinct select setter
	 *
	 * @param bool		$ignore
	 * @return self
	 */
	public function distinct( $distinct = true )
	{
		$this->distinct = $distinct; return $this;
	}

	/**
	 * Set the select fields
	 *
	 * @param array 		$values
	 * @return self
	 */
	public function fields( $fields )
	{
		if ( !is_array( $fields ) && !is_null( $fields ) )
		{
			$fields = array( $fields );
		}
		
		// do nothing if we get nothing
		if ( empty( $fields ) || $fields == array( '*' ) )
		{
			$this->fields = array(); return $this;
		}
		
		$this->fields = $fields;

		// return self so we can continue running the next function
		return $this;
	}
	
	/**
	 * Add select fields
	 *
	 * @param array 		$values
	 * @return self
	 */
	public function add_fields( $fields )
	{
		if ( !is_array( $fields ) )
		{
			$fields = array( $fields );
		}
		
		// merge the new values with the existing ones.
		$this->fields = array_merge( $this->fields, $fields ); 
	
		// return self so we can continue running the next function
		return $this;
	}

	/**
	 * Set the order parameters
	 *
	 * @param mixed 	$cols
	 * @param string	$order
	 * @return self
	 */
	public function order_by( $cols, $order = 'asc' ) 
	{
		if ( !is_array( $cols ) ) 
		{
			$this->orders[] = array( $cols, $order ); return $this;
		}
		else
		{
			foreach( $cols as $key => $col ) 
			{
				if ( is_numeric( $key ) ) 
				{
					$this->orders[] = array( $col, $order );	
				} 
				else 
				{
					$this->orders[] = array( $key, $col );
				}
			}
		}
		
		return $this;
	}
	
	/**
	 * Add group by stuff
	 *
	 * @param mixed 		$key		By passing an array you can add multiple groups
	 * @return self
	 */
	public function group_by( $key ) 
	{
		if ( !is_array( $key ) )
		{
			$key = array( $key );	
		}
		
		foreach( $key as $group_key )
		{
			$this->groups[] = $group_key;
		}
		
		return $this;
	}
	
	/**
	 * Forward a result value as array key
	 *
	 * @param string|bool		$key
	 * @return self
	 */
	public function forward_key( $key = true )
	{
		if ( $key === false )
		{
			$this->forward_key = false;
		}
		elseif ( $key === true )
		{
			$this->forward_key = \ClanCats::$config->get( 'database.default_primary_key', 'id' );
		}
		else
		{
			$this->forward_key = $key;
		}
		
		return $this;
	}
	
	/**
	 * Group results by a column
	 *
	 * example:
	 *     array(
	 *         'name' => 'John', 'age' => 18,
	 *     ),
	 *     array(
	 *         'name' => 'Jeff', 'age' => 32,
	 *     ),
	 *     array(
	 *         'name' => 'Jenny', 'age' => 18,
	 *     ),
	 * To:
	 *     '18' => array(
	 *          array( 'name' => 'John', 'age' => 18 ),
	 *          array( 'name' => 'Jenny', 'age' => 18 ),
	 *     ),
	 *     '32' => array(
	 *          array( 'name' => 'Jeff', 'age' => 32 ),
	 *     ),
	 *
	 * @param string|bool		$key
	 * @return self
	 */
	public function group_result( $key )
	{
		if ( $key === false )
		{
			$this->group_result = false;
		}
		else
		{
			$this->group_result = $key;
		}
		
		return $this;
	}
	
	/**
	 * Set the fetch handler for this query
	 *
	 * @param callback 		$callback
	 * @return self
	 */
	public function fetch_handler( $callback )
	{
		$this->fetch_handler = $callback; return $this;
	}
	
	/**
	 * Build the query to a string
	 *
	 * @return string
	 */
	public function build()
	{
		// Some task's like clearing the query parameters
		// are handeld by the parent build function.
		parent::build();
		
		// Lets run the Builder by passing the current query object
		return $this->handler->builder()->compile_select( $this );
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
		
		// if there is a special fetch handler defined pass him all the 
		// needed parameters and retrive the results
		if ( !is_null( $this->fetch_handler ) )
		{
			$results = call_user_func_array( $this->fetch_handler, array(
				&$this
			));
		}
		// otherwise simply do the default select fetch
		else 
		{
			$results = $this->handler->fetch( $this->build(), $this->handler->builder()->parameters, $this->fetch_arguments );
		}
		
		// In case we should forward a key means using a value
		// from every result as array key.
		if ( $this->forward_key !== false )
		{
			$raw_results = $results;
			$results = array();
			
			if ( in_array( 'assoc', $this->fetch_arguments ) )
			{
				foreach( $raw_results as $result )
				{
					$results[$result[$this->forward_key]] = $result;
				}
			}
			else
			{
				foreach( $raw_results as $result )
				{
					$results[$result->{$this->forward_key}] = $result;
				}
			}
		}
		
		// Group the results by a value
		if ( $this->group_result !== false )
		{
			$raw_results = $results;
			$results = array();
			
			if ( in_array( 'assoc', $this->fetch_arguments ) )
			{
				foreach( $raw_results as $key => $result )
				{
					$results[$result[$this->group_result]][$key] = $result;
				}
			}
			else
			{
				foreach( $raw_results as $key => $result )
				{
					$results[$result->{$this->group_result}][$key] = $result;
				}
			}
		}
		
		// when the limit is 1 we are going to return the 
		// result directly
		if ( $this->limit === 1 )
		{
			return reset( $results );
		}
		
		return $results;
	}
	
	/**
	 * Get one result sets limit to 1 and executes
	 *
	 * @param string			$name
	 * @return mixed
	 */
	public function one( $name = null ) 
	{
		return $this->limit( 0, 1 )->run( $name );
	}
	
	/**
	 * Find something, means select one record by key
	 *
	 * @param int			$id
	 * @param string			$key
	 * @param string			$name
	 * @return mixed
	 */
	public function find( $id, $key = null, $name = null ) 
	{
		if ( is_null( $key ) )
		{
			$key = Query::$_default_key;
		}
		
		return $this->where( $key, $id )->one( $name );
	}
	
	/**
	 * Get the first result by key
	 *
	 * @param string			$key
	 * @param string			$name
	 * @return mixed
	 */
	public function first( $key = null, $name = null ) 
	{
		if ( is_null( $key ) )
		{
			$key = Query::$_default_key;
		}
		
		return $this->order_by( $key, 'asc' )->one( $name );
	}
	
	/**
	 * Get the last result by key
	 *
	 * @param string			$key
	 * @param string			$name
	 * @return mixed
	 */
	public function last( $key = null, $name = null ) 
	{
		if ( is_null( $key ) )
		{
			$key = Query::$_default_key;
		}
		
		return $this->order_by( $key, 'desc' )->one( $name );
	}
	
	/**
	 * Just get a single value from the db
	 *
	 * @param string			$column
	 * @param string			$name
	 * @return mixed
	 */
	public function column( $column, $name = null ) 
	{
		return $this->fields( $column )->one( $name )->$column;
	}
	
	/**
	 * Just return the count result 
	 *
	 * @param string 	$db
	 * @return int
	 */
	public function count( $name = null ) 
	{
		return (int) $this->column( DB::raw( "COUNT(*)" ), $name );
	}
}