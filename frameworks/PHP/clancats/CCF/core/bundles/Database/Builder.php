<?php namespace DB;
/**
 * The Query builder
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Builder
{
	/**
	 * The query parameters 
	 *
	 * @var array
	 */
	public $parameters = array();
	
	/**
	 * The escape pattern escapes table column names etc. 
	 * select * from `table`...
	 *
	 * @var string
	 */
	protected $escape_pattern = '`%s`';
	
	/**
	 * Clear all set parameters
	 *
	 * @return void
	 */
	public function clear_parameters()
	{
		$this->parameters = array();
	}
	
	/**
	 * Adds a parameter to the builder
	 *
	 * @return void
	 */
	public function add_parameter( $value )
	{
		$this->parameters[] = $value;
	}
	
	/**
	 * creates an parameter and adds it
	 *
	 * @param mixed 		$value
	 * @return string
	 */
	public function param( $value )
	{
		if ( !\DB::is_expression( $value ) )
		{
			$this->add_parameter( $value ); return '?';
		}
		return $value;
	}
	
	/**
	 * Filters the parameters removes the keys and Expressions
	 *
	 * @param array 		$parameters
	 * @return array
	 */
	public function filter_parameters( $parameters )
	{
		return array_values( array_filter( $parameters, function( $item ) {
			return !\DB::is_expression( $item );
		}));
	}
	
	/**
	 * Escape / wrap an string for sql
	 *
	 * @param string|Expression        $string
	 */
	public function escape( $string )
	{	
		if ( \DB::is_expression( $string ) ) 
		{
			return $string->value;
		}
		
		// the string might contain an 'as' statement that we wil have to split.
		if ( strpos( $string, ' as ' ) !== false )
		{
			$string = explode( ' as ', $string );
			return $this->escape( trim( $string[0] ) ).' as '. $this->escape( trim( $string[1] ) );
		}
		
		// it also might contain dott seperations we have to split
		if ( strpos( $string, '.' ) !== false )
		{
			$string = explode( '.', $string );
			
			foreach( $string as $key => $item )
			{
				$string[$key] = $this->escape_string( $item );
			}
			
			return implode( '.' , $string );
		}
		
		return $this->escape_string( $string );
	}
	
	/**
	 * Escape a single string without checking for as and dots
	 *
	 * @param string 	$string
	 * @return string
	 */
	public function escape_string( $string )
	{
		return sprintf( $this->escape_pattern, $string );
	}
	
	/**
	 * Escape an array of items an seprate them with a comma
	 *
	 * @param array 		$array
	 * @return string
	 */
	public function escape_list( $array )
	{
		foreach( $array as $key => $item )
		{
			$array[$key] = $this->escape( $item );
		}
		
		return implode( ', ', $array );
	}
	
	/**
	 * Escape the table 
	 *
	 * @param Query		$query
	 * @return string
	 */
	public function escape_table( &$query )
	{
		$table = $query->table;
		
		if ( is_array( $table ) )
		{
			reset($table); $table = key($table).' as '.$table[key($table)];
		}
		
		return $this->escape( $table );
	}
	
	/**
	 * Convert data to parameters and bind them to the query
	 *
	 * @param array 		$params
	 * @return string
	 */
	public function parameterize( $params )
	{
		foreach( $params as $key => $param )
		{
			$params[$key] = $this->param( $param );
		}
		
		return implode( ', ', $params );
	}

	/**
	 * Build an insert query
	 *
	 * @param Query     $query
	 * @return string
	 */
	public function compile_insert( &$query )
	{
		$build = ( $query->ignore ? 'insert ignore' : 'insert' ).' into '.$this->escape_table( $query ).' ';
		
		$value_collection = $query->values;
		
		// Get the array keys from the first array in the collection.
		// We use them as insert keys.
		$build .= '('.$this->escape_list( array_keys( reset( $value_collection ) ) ).') values ';
		
		// add the array values.
		foreach( $value_collection as $values )
		{
			$build .= '('.$this->parameterize( $values ).'), ';
		}
		
		// cut the last comma away
		return substr( $build, 0, -2 );
	}
	
	/**
	 * Build an update query
	 *
	 * @param Query     $query
	 * @return string
	 */
	public function compile_update( &$query )
	{
		$build = 'update '.$this->escape_table( $query ).' set ';
		
		// add the array values.
		foreach( $query->values as $key => $value )
		{
			$build .= $this->escape( $key ).' = '.$this->param( $value ).', ';
		}
		
		$build = substr( $build, 0, -2 );
		$build .= $this->compile_where( $query );
		$build .= $this->compile_limit( $query );
		
		// cut the last comma away
		return $build;
	}
	
	/**
	 * Build an delete query
	 *
	 * @param Query     $query
	 * @return string
	 */
	public function compile_delete( &$query )
	{
		$build = 'delete from '.$this->escape_table( $query );
		
		$build .= $this->compile_where( $query );
		$build .= $this->compile_limit( $query );
		
		// cut the last comma away
		return $build;
	}
	
	/**
	 * Build a select
	 *
	 * @param Query     $query
	 * @return string
	 */
	public function compile_select( &$query )
	{
		$build = ( $query->distinct ? 'select distinct' : 'select' ).' ';
		
		if ( !empty( $query->fields ) )
		{
			foreach( $query->fields as $key => $field )
			{
				if ( !is_numeric( $key ) )
				{
					$build .= $this->escape( $key ).' as '.$this->escape( $field );
				}
				elseif ( is_array( $field ) )
				{
					$build .= $this->escape( $field[0] ).' as '.$this->escape( $field[1] );
				}
				else 
				{
					$build .= $this->escape( $field );
				}
				$build .= ', ';
			}
			
			$build = substr( $build, 0, -2 );
		}
		else 
		{
			$build .= '*';
		}
		
		// append the table
		$build .= ' from '.$this->escape_table( $query );
		
		// build the where stuff
		$build .= $this->compile_where( $query );
		$build .= $this->compile_group( $query );
		$build .= $this->compile_order( $query );
		$build .= $this->compile_limit_with_offset( $query );
		
		return $build;
	}
	
	/**
	 * Build the where part
	 *
	 * @param Query 		$query
	 * @return string
	 */
	public function compile_where( &$query )
	{
		$build = '';
		
		foreach( $query->wheres as $where ) 
		{	
			// to make nested wheres possible you can pass an closure 
			// wich will create a new query where you can add your nested wheres
			if ( !isset( $where[2] ) && is_closure( $where[1] ) )
			{
				$sub_query = new Query;
				
				call_user_func( $where[1], $sub_query );
				
				$build .= ' '.$where[0].' ( '.substr( $this->compile_where( $sub_query ), 7 ).' )';
				
				foreach( $sub_query->parameters as $param )
				{
					$this->add_parameter( $param );
				}
				
				continue;
			}
			
			// when we have an array as where values we have 
			// to parameterize them
			if ( is_array( $where[3] ) ) 
			{
				$where[3] = '('.$this->parameterize( $where[3] ).')';
			}
			else
			{
				$where[3] = $this->param( $where[3] );
			}
			
			// we always need to escepe where 1 wich referrs to the key
			$where[1] = $this->escape( $where[1] );
			
			// implode the beauty
			$build .= ' '.implode( ' ', $where );
		}
		
		return $build;
	}
	
	/**
	 * Build the limit and offset part
	 *
	 * @param Query 		$query
	 * @return string
	 */
	public function compile_limit_with_offset( &$query ) 
	{
		if ( is_null( $query->limit ) )
		{
			return "";
		}
		
		return ' limit '.( (int) $query->offset ).', '.( (int) $query->limit );
	}
	
	/**
	 * Build the limit and offset part
	 *
	 * @param Query 		$query
	 * @return string
	 */
	public function compile_limit( &$query ) 
	{
		if ( is_null( $query->limit ) )
		{
			return "";
		}
		
		return ' limit '.( (int) $query->limit );
	}
	
	/**
	 * Build the order by statement
	 *
	 * @param Query 		$query
	 * @return string
	 */
	protected function compile_order( &$query ) 
	{
		if ( empty( $query->orders ) )
		{
			return '';
		}
		
		$build = " order by ";
		
		foreach( $query->orders as $order ) 
		{
			$build .= $this->escape( $order[0] ).' '.$order[1].', ';
		}
		
		return substr( $build, 0, -2 ); 
	}
	
	/**
	 * Build the gorup by statemnet
	 *
	 * @param Query 		$query
	 * @return string
	 */
	protected function compile_group( &$query ) 
	{
		if ( empty( $query->groups ) )
		{
			return '';
		}
		
		return ' group by '.$this->escape_list( $query->groups );
	}
}