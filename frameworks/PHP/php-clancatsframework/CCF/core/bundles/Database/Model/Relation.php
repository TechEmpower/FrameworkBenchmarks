<?php namespace DB;
/**
 * DB Model Relation 
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Model_Relation
{
	/**
	 * The current relation query
	 *
	 * @var DB\Query
	 */
	public $query = null;
	
	/**
	 * The key on the related model
	 *
	 * @var string
	 */
	public $foreign_key = null;
	
	/**
	 * The key on the local model
	 *
	 * @var string
	 */
	public $local_key = null;
	
	/**
	 * The related model
	 *
	 * @var DB\Model
	 */
	public $foreign_model = null;
	
	/**
	 * The model where the request came from
	 *
	 * @var DB\Model
	 */
	public $local_model = null;
	
	/**
	 * Should this relationship deliver a single item
	 *
	 * @var bool
	 */ 
	public $singleton = true;
	
	/**
	 * Create new relationship instance
	 *
	 * @param DB\Model			$model
	 * @param string				$foreign_key
	 * @param string 			$key
	 * @return void
	 */
	public function __construct( $local, $foreign, $foreign_key, $local_key )
	{
		$this->local_model = $local;
		$this->foreign_model = $foreign;
		
		if ( !class_exists( $foreign ) )
		{
			throw new ModelException( 'Invalid class "'.$foreign.'" given for relationship.' );
		}
		
		// set or create the for foreign_key
		if ( !is_null( $foreign_key ) )
		{
			$this->foreign_key = $foreign_key;
		} else {
			$this->foreign_key = $this->get_foreign_key();
		}
		
		// set or create the for local_key
		if ( !is_null( $local_key ) )
		{
			$this->local_key = $local_key;
		} else {
			$this->local_key = $this->get_local_key();
		}
		
		// create a basic query objcet		
		$this->query = $this->create_query( $foreign );
		
		// prepare the query 
		$this->prepare_query();
	}
	
	/**
	 * Get foreign key from the models
	 *
	 * @return void
	 */
	protected function get_foreign_key()
	{
		return basename( $this->local_model->model_settings( 'name' ) ).
			'_'.$this->local_model->model_settings( 'primary_key' );
	}
	
	/**
	 * Get foreign key from the models
	 *
	 * @return void
	 */
	protected function get_local_key()
	{
		return call_user_func( $this->foreign_model.'::_model', 'primary_key' );
	}
	
	/**
	 * Creates a query object based on a given model
	 *
	 * @param DB\Model			$model
	 * @return DB\Query
	 */
	protected function create_query( $model )
	{
		return call_user_func( $model.'::select' )->forward_key();
	}
	
	/**
	 * Prepare the query object
	 *
	 * @return void
	 */
	protected function prepare_query() {}
	
	/**
	 * Prepare the query with collection select
	 *
	 * @param array 			$collection
	 * @return void
	 */
	protected function collection_query( &$collection ) 
	{
		$local_keys = array();
		
		foreach( $collection as $item )
		{
			$local_keys[] = $item->raw( $this->local_key );
		}
		
		// set the correct collection where
		$this->query->wheres = array();
		$this->query->where( $this->foreign_key, 'in', $local_keys );
		$this->query->group_result( $this->foreign_key );
		
		$this->query->limit( null );
	}
	
	/**
	 * Prepare the query with collection select
	 *
	 * @param string 			$relation
	 * @param array  			$collection
	 * @param callback			$callback
	 * @return void
	 */
	public function collection_assign( $relation, &$collection, $callback = null ) 
	{
		// make the query
		$this->collection_query( $collection );
		
		call_user_func_array( $callback, array( &$this->query ) );
		
		// get the reults
		$results = $this->query->run();
		
		foreach( $collection as $item )
		{
			if ( $this->singleton )
			{
				$item->raw_set( $relation, reset( $results[ $item->raw( $this->local_key ) ] ) );
			}
			else
			{
				$item->raw_set( $relation, $results[ $item->raw( $this->local_key ) ] );
			}
		}
	}
	
	/**
	 * Forward the query functionality
	 *
	 * @param string			$method
	 * @param array 			$params
	 * @return mixed
	 */
	public function __call( $method, $params )
	{
		$results = call_user_func_array( array( $this->query, $method ), $params );
		
		// because we want to keep the realtionship object
		// we have to return this if its still the query
		if ( $results === $this->query )
		{
			return $this;
		}
		
		return $results;
	}
}