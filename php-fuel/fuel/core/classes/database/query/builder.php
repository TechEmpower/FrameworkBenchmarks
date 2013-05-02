<?php
/**
 * Database query builder.
 *
 * @package    Fuel/Database
 * @category   Query
 * @author     Kohana Team
 * @copyright  (c) 2008-2009 Kohana Team
 * @license    http://kohanaphp.com/license
 */

namespace Fuel\Core;

abstract class Database_Query_Builder extends \Database_Query
{

	/**
	 * Compiles an array of JOIN statements into an SQL partial.
	 *
	 * @param   object  Database instance
	 * @param   array   join statements
	 * @return  string
	 */
	protected function _compile_join(\Database_Connection$db, array $joins)
	{
		$statements = array();

		foreach ($joins as $join)
		{
			// Compile each of the join statements
			$statements[] = $join->compile($db);
		}

		return implode(' ', $statements);
	}

	/**
	 * Compiles an array of conditions into an SQL partial. Used for WHERE
	 * and HAVING.
	 *
	 * @param   object  Database instance
	 * @param   array   condition statements
	 * @return  string
	 */
	protected function _compile_conditions(\Database_Connection$db, array $conditions)
	{
		$last_condition = NULL;

		$sql = '';
		foreach ($conditions as $group)
		{
			// Process groups of conditions
			foreach ($group as $logic => $condition)
			{
				if ($condition === '(')
				{
					if ( ! empty($sql) AND $last_condition !== '(')
					{
						// Include logic operator
						$sql .= ' '.$logic.' ';
					}

					$sql .= '(';
				}
				elseif ($condition === ')')
				{
					$sql .= ')';
				}
				else
				{
					if ( ! empty($sql) AND $last_condition !== '(')
					{
						// Add the logic operator
						$sql .= ' '.$logic.' ';
					}

					// Split the condition
					list($column, $op, $value) = $condition;

					if ($value === NULL)
					{
						if ($op === '=')
						{
							// Convert "val = NULL" to "val IS NULL"
							$op = 'IS';
						}
						elseif ($op === '!=')
						{
							// Convert "val != NULL" to "valu IS NOT NULL"
							$op = 'IS NOT';
						}
					}

					// Database operators are always uppercase
					$op = strtoupper($op);

					if (($op === 'BETWEEN' OR $op === 'NOT BETWEEN') AND is_array($value))
					{
						// BETWEEN always has exactly two arguments
						list($min, $max) = $value;

						if (is_string($min) AND array_key_exists($min, $this->_parameters))
						{
							// Set the parameter as the minimum
							$min = $this->_parameters[$min];
						}

						if (is_string($max) AND array_key_exists($max, $this->_parameters))
						{
							// Set the parameter as the maximum
							$max = $this->_parameters[$max];
						}

						// Quote the min and max value
						$value = $db->quote($min).' AND '.$db->quote($max);
					}
					else
					{
						if (is_string($value) AND array_key_exists($value, $this->_parameters))
						{
							// Set the parameter as the value
							$value = $this->_parameters[$value];
						}

						// Quote the entire value normally
						$value = $db->quote($value);
					}

					// Append the statement to the query
					$sql .= $db->quote_identifier($column).' '.$op.' '.$value;
				}

				$last_condition = $condition;
			}
		}

		return $sql;
	}

	/**
	 * Compiles an array of set values into an SQL partial. Used for UPDATE.
	 *
	 * @param   object  Database instance
	 * @param   array   updated values
	 * @return  string
	 */
	protected function _compile_set(\Database_Connection$db, array $values)
	{
		$set = array();
		foreach ($values as $group)
		{
			// Split the set
			list ($column, $value) = $group;

			// Quote the column name
			$column = $db->quote_identifier($column);

			if (is_string($value) AND array_key_exists($value, $this->_parameters))
			{
				// Use the parameter value
				$value = $this->_parameters[$value];
			}

			$set[$column] = $column.' = '.$db->quote($value);
		}

		return implode(', ', $set);
	}

	/**
	 * Compiles an array of ORDER BY statements into an SQL partial.
	 *
	 * @param   object  Database instance
	 * @param   array   sorting columns
	 * @return  string
	 */
	protected function _compile_order_by(\Database_Connection$db, array $columns)
	{
		$sort = array();
		foreach ($columns as $group)
		{
			list ($column, $direction) = $group;

			if ( ! empty($direction))
			{
				// Make the direction uppercase
				$direction = ' '.strtoupper($direction);
			}

			$sort[] = $db->quote_identifier($column).$direction;
		}

		return 'ORDER BY '.implode(', ', $sort);
	}

	/**
	 * Reset the current builder status.
	 *
	 * @return  $this
	 */
	abstract public function reset();

} // End Database_Query_Builder
