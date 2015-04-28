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
class Model_Relation_BelongsTo extends Model_Relation
{
	/**
	 * Get foreign key from the models
	 *
	 * @return void
	 */
	protected function get_local_key()
	{
		return basename( call_user_func( $this->foreign_model.'::_model', 'name' ) ).'_'.call_user_func( $this->foreign_model.'::_model', 'primary_key' );
	}
	
	/**
	 * Get foreign key from the models
	 *
	 * @return void
	 */
	protected function get_foreign_key()
	{
		return call_user_func( $this->foreign_model.'::_model', 'primary_key' );
	}
	
	/**
	 * Prepare the query object
	 *
	 * @return void
	 */
	protected function prepare_query() 
	{
		$this->query->where( $this->foreign_key, $this->local_model->{$this->local_key} )
			->limit( 1 );
	}
}