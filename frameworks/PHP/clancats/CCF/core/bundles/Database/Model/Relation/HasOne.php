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
class Model_Relation_HasOne extends Model_Relation
{
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