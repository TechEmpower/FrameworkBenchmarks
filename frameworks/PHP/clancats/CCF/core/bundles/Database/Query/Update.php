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
class Query_Update extends Query
{
	/**
	 * values container
	 *
	 * @var array 
	 */
	public $values = array();
	
	/**
	 * Add set values to the update query
	 *
	 * @param string|array 		$param1
	 * @param mixed				$param2
	 * @return self
	 */
	public function set( $param1, $param2 = null )
	{
		// do nothing if we get nothing
		if ( empty( $param1 ) )
		{
			return $this;
		}
		
		// when param 2 is not null we assume that only one set is passed
		// like: set( 'name', 'Lu' ); instead of set( array( 'name' => 'Lu' ) );
		if ( !is_null( $param2 ) )
		{
			$param1 = array( $param1 => $param2 );
		}
		
		// merge the new values with the existing ones.
		$this->values = array_merge( $this->values, $param1 ); 
		
		// return self so we can continue running the next function
		return $this;
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
		return $this->handler->builder()->compile_update( $this );
	}
}