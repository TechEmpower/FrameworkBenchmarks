<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * Database query builder for JOIN statements. See [Query Builder](/database/query/builder) for usage and examples.
 *
 * @package    Kohana/Database
 * @category   Query
 * @author     Kohana Team
 * @copyright  (c) 2008-2009 Kohana Team
 * @license    http://kohanaphp.com/license
 */
class Kohana_Database_Query_Builder_Join extends Database_Query_Builder {

	// Type of JOIN
	protected $_type;

	// JOIN ...
	protected $_table;

	// ON ...
	protected $_on = array();

	// USING ...
	protected $_using = array();

	/**
	 * Creates a new JOIN statement for a table. Optionally, the type of JOIN
	 * can be specified as the second parameter.
	 *
	 * @param   mixed   $table  column name or array($column, $alias) or object
	 * @param   string  $type   type of JOIN: INNER, RIGHT, LEFT, etc
	 * @return  void
	 */
	public function __construct($table, $type = NULL)
	{
		// Set the table to JOIN on
		$this->_table = $table;

		if ($type !== NULL)
		{
			// Set the JOIN type
			$this->_type = (string) $type;
		}
	}

	/**
	 * Adds a new condition for joining.
	 *
	 * @param   mixed   $c1  column name or array($column, $alias) or object
	 * @param   string  $op  logic operator
	 * @param   mixed   $c2  column name or array($column, $alias) or object
	 * @return  $this
	 */
	public function on($c1, $op, $c2)
	{
		if ( ! empty($this->_using))
		{
			throw new Kohana_Exception('JOIN ... ON ... cannot be combined with JOIN ... USING ...');
		}

		$this->_on[] = array($c1, $op, $c2);

		return $this;
	}

	/**
	 * Adds a new condition for joining.
	 *
	 * @param   string  $columns  column name
	 * @return  $this
	 */
	public function using($columns)
	{
		if ( ! empty($this->_on))
		{
			throw new Kohana_Exception('JOIN ... ON ... cannot be combined with JOIN ... USING ...');
		}

		$columns = func_get_args();

		$this->_using = array_merge($this->_using, $columns);

		return $this;
	}

	/**
	 * Compile the SQL partial for a JOIN statement and return it.
	 *
	 * @param   mixed  $db  Database instance or name of instance
	 * @return  string
	 */
	public function compile($db = NULL)
	{
		if ( ! is_object($db))
		{
			// Get the database instance
			$db = Database::instance($db);
		}

		if ($this->_type)
		{
			$sql = strtoupper($this->_type).' JOIN';
		}
		else
		{
			$sql = 'JOIN';
		}

		// Quote the table name that is being joined
		$sql .= ' '.$db->quote_table($this->_table);

		if ( ! empty($this->_using))
		{
			// Quote and concat the columns
			$sql .= ' USING ('.implode(', ', array_map(array($db, 'quote_column'), $this->_using)).')';
		}
		else
		{
			$conditions = array();
			foreach ($this->_on as $condition)
			{
				// Split the condition
				list($c1, $op, $c2) = $condition;

				if ($op)
				{
					// Make the operator uppercase and spaced
					$op = ' '.strtoupper($op);
				}

				// Quote each of the columns used for the condition
				$conditions[] = $db->quote_column($c1).$op.' '.$db->quote_column($c2);
			}

			// Concat the conditions "... AND ..."
			$sql .= ' ON ('.implode(' AND ', $conditions).')';
		}

		return $sql;
	}

	public function reset()
	{
		$this->_type =
		$this->_table = NULL;

		$this->_on = array();
	}

} // End Database_Query_Builder_Join
