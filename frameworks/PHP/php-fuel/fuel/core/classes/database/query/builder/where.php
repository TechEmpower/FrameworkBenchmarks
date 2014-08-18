<?php
/**
 * Database query builder for WHERE statements.
 *
 * @package    Fuel/Database
 * @category   Query
 * @author     Kohana Team
 * @copyright  (c) 2008-2009 Kohana Team
 * @license    http://kohanaphp.com/license
 */

namespace Fuel\Core;

abstract class Database_Query_Builder_Where extends \Database_Query_Builder
{

	// WHERE ...
	protected $_where = array();

	// ORDER BY ...
	protected $_order_by = array();

	// LIMIT ...
	protected $_limit = NULL;

	/**
	 * Alias of and_where()
	 *
	 * @return  $this
	 */
	public function where()
	{
		return call_user_func_array(array($this, 'and_where'), func_get_args());
	}

	/**
	 * Creates a new "AND WHERE" condition for the query.
	 *
	 * @param   mixed   column name or array($column, $alias) or object
	 * @param   string  logic operator
	 * @param   mixed   column value
	 * @return  $this
	 */
	public function and_where($column, $op = null, $value = null)
	{
		if($column instanceof \Closure)
		{
			$this->and_where_open();
			$column($this);
			$this->and_where_close();
			return $this;
		}

		if (is_array($column))
		{
			foreach ($column as $key => $val)
			{
				if (is_array($val))
				{
					$this->and_where($val[0], $val[1], $val[2]);
				}
				else
				{
					$this->and_where($key, '=', $val);
				}
			}
		}
		else
		{
			if(func_num_args() === 2)
			{
				$value = $op;
				$op = '=';
			}
			$this->_where[] = array('AND' => array($column, $op, $value));
		}

		return $this;
	}

	/**
	 * Creates a new "OR WHERE" condition for the query.
	 *
	 * @param   mixed   column name or array($column, $alias) or object
	 * @param   string  logic operator
	 * @param   mixed   column value
	 * @return  $this
	 */
	public function or_where($column, $op = null, $value = null)
	{
		if($column instanceof \Closure)
		{
			$this->or_where_open();
			$column($this);
			$this->or_where_close();
			return $this;
		}

		if (is_array($column))
		{
			foreach ($column as $key => $val)
			{
				if (is_array($val))
				{
					$this->or_where($val[0], $val[1], $val[2]);
				}
				else
				{
					$this->or_where($key, '=', $val);
				}
			}
		}
		else
		{
			if(func_num_args() === 2)
			{
				$value = $op;
				$op = '=';
			}
			$this->_where[] = array('OR' => array($column, $op, $value));
		}
		return $this;
	}

	/**
	 * Alias of and_where_open()
	 *
	 * @return  $this
	 */
	public function where_open()
	{
		return $this->and_where_open();
	}

	/**
	 * Opens a new "AND WHERE (...)" grouping.
	 *
	 * @return  $this
	 */
	public function and_where_open()
	{
		$this->_where[] = array('AND' => '(');

		return $this;
	}

	/**
	 * Opens a new "OR WHERE (...)" grouping.
	 *
	 * @return  $this
	 */
	public function or_where_open()
	{
		$this->_where[] = array('OR' => '(');

		return $this;
	}

	/**
	 * Closes an open "AND WHERE (...)" grouping.
	 *
	 * @return  $this
	 */
	public function where_close()
	{
		return $this->and_where_close();
	}

	/**
	 * Closes an open "AND WHERE (...)" grouping.
	 *
	 * @return  $this
	 */
	public function and_where_close()
	{
		$this->_where[] = array('AND' => ')');

		return $this;
	}

	/**
	 * Closes an open "OR WHERE (...)" grouping.
	 *
	 * @return  $this
	 */
	public function or_where_close()
	{
		$this->_where[] = array('OR' => ')');

		return $this;
	}

	/**
	 * Applies sorting with "ORDER BY ..."
	 *
	 * @param   mixed   column name or array($column, $alias) or object
	 * @param   string  direction of sorting
	 * @return  $this
	 */
	public function order_by($column, $direction = null)
	{
		$this->_order_by[] = array($column, $direction);

		return $this;
	}

	/**
	 * Return up to "LIMIT ..." results
	 *
	 * @param   integer  maximum results to return
	 * @return  $this
	 */
	public function limit($number)
	{
		$this->_limit = (int) $number;

		return $this;
	}

} // End Database_Query_Builder_Where
