<?php
/**
 * Database query builder for INSERT statements.
 *
 * @package    Fuel/Database
 * @category   Query
 * @author     Kohana Team
 * @copyright  (c) 2008-2009 Kohana Team
 * @license    http://kohanaphp.com/license
 */

namespace Fuel\Core;

class Database_Query_Builder_Insert extends \Database_Query_Builder
{

	// INSERT INTO ...
	protected $_table;

	// (...)
	protected $_columns = array();

	// VALUES (...)
	protected $_values = array();

	/**
	 * Set the table and columns for an insert.
	 *
	 * @param   mixed  table name or array($table, $alias) or object
	 * @param   array  column names
	 * @return  void
	 */
	public function __construct($table = NULL, array $columns = NULL)
	{
		if ($table)
		{
			// Set the inital table name
			$this->_table = $table;
		}

		if ($columns)
		{
			// Set the column names
			$this->_columns = $columns;
		}

		// Start the query with no SQL
		return parent::__construct('', \DB::INSERT);
	}

	/**
	 * Sets the table to insert into.
	 *
	 * @param   mixed  table name or array($table, $alias) or object
	 * @return  $this
	 */
	public function table($table)
	{
		$this->_table = $table;

		return $this;
	}

	/**
	 * Set the columns that will be inserted.
	 *
	 * @param   array  column names
	 * @return  $this
	 */
	public function columns(array $columns)
	{
		$this->_columns = array_merge($this->_columns, $columns);

		return $this;
	}

	/**
	 * Adds values. Multiple value sets can be added.
	 *
	 * @param   array   values list
	 * @param   ...
	 * @return  $this
	 */
	public function values(array $values)
	{
		if ( ! is_array($this->_values))
		{
			throw new \FuelException('INSERT INTO ... SELECT statements cannot be combined with INSERT INTO ... VALUES');
		}

		// Get all of the passed values
		$values = func_get_args();

		$this->_values = array_merge($this->_values, $values);

		return $this;
	}

	/**
	 * This is a wrapper function for calling columns() and values().
	 *
	 * @param	array	column value pairs
	 * @return	$this
	 */
	public function set(array $pairs)
	{
		$this->columns(array_keys($pairs));
		$this->values($pairs);

		return $this;
	}

	/**
	 * Use a sub-query to for the inserted values.
	 *
	 * @param   object  Database_Query of SELECT type
	 * @return  $this
	 */
	public function select(Database_Query $query)
	{
		if ($query->type() !== \DB::SELECT)
		{
			throw new \FuelException('Only SELECT queries can be combined with INSERT queries');
		}

		$this->_values = $query;

		return $this;
	}

	/**
	 * Compile the SQL query and return it.
	 *
	 * @param   mixed  Database instance or instance name
	 * @return  string
	 */
	public function compile($db = null)
	{
		if ( ! $db instanceof \Database_Connection)
		{
			// Get the database instance
			$db = \Database_Connection::instance($db);
		}

		// Start an insertion query
		$query = 'INSERT INTO '.$db->quote_table($this->_table);

		// Add the column names
		$query .= ' ('.implode(', ', array_map(array($db, 'quote_identifier'), $this->_columns)).') ';

		if (is_array($this->_values))
		{
			// Callback for quoting values
			$quote = array($db, 'quote');

			$groups = array();
			foreach ($this->_values as $group)
			{
				foreach ($group as $i => $value)
				{
					if (is_string($value) AND isset($this->_parameters[$value]))
					{
						// Use the parameter value
						$group[$i] = $this->_parameters[$value];
					}
				}

				$groups[] = '('.implode(', ', array_map($quote, $group)).')';
			}

			// Add the values
			$query .= 'VALUES '.implode(', ', $groups);
		}
		else
		{
			// Add the sub-query
			$query .= (string) $this->_values;
		}

		return $query;
	}

	public function reset()
	{
		$this->_table = NULL;

		$this->_columns = array();
		$this->_values  = array();

		$this->_parameters = array();

		return $this;
	}

} // End Database_Query_Builder_Insert
