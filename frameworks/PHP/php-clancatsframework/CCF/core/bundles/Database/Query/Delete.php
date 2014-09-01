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
class Query_Delete extends Query
{
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
		return $this->handler->builder()->compile_delete( $this );
	}
}