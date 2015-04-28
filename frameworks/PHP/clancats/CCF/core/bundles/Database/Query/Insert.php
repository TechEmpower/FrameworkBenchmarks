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
class Query_Insert extends Query
{
	/**
	 * values container
	 *
	 * @var array 
	 */
	public $values = array();
	
	/**
 	 * make an insert ignore
	 *
	 * @var bool 
	 */
	public $ignore = false;
	
	/**
	 * Insert ignore setter
	 *
	 * @param bool		$ignore
	 * @return void
	 */
	public function ignore( $ignore = true )
	{
		$this->ignore = $ignore; return $this;
	}
	
	/**
	 * Add values to the insert
	 *
	 * @param array 		$values
	 * @return void
	 */
	public function values( array $values )
	{
		// do nothing if we get nothing
		if ( empty( $values ) )
		{
			return $this;
		}
		
		// check if the the passed array is a collection.
		// because we want to be able to insert bulk values.
		if ( !\CCArr::is_collection( $values ) )
		{
			$values = array( $values );
		}
		
		// because we could recive the arrays in diffrent order 
		// we have to sort them by their key.
		foreach( $values as $key => $value )
		{
			ksort( $value ); $values[$key] = $value;
		}
		
		// merge the new values with the existing ones.
		$this->values = array_merge( $this->values, $values ); 
		
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
		return $this->handler->builder()->compile_insert( $this );
	}
}