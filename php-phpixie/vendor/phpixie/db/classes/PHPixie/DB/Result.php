<?php

namespace PHPixie\DB;

/**
 * Allows to access database results in a unified way and
 * provides iterator support, so it can be used inside loops like 'foreach'
 * @package Database
 */
abstract class Result implements \Iterator
{

	/**
	 * Current row number
	 * @var integer
	 */
	protected $_position = -1;

	/**
	 * Database result object
	 * @var mixed
	 */
	protected $_result;

	/**
	 * Current row
	 * @var object
	 */
	protected $_row;

	/**
	 * If at least one row has been fetched
	 * @var object
	 */
	protected $_fetched = false;

	/**
	 * Returns current row
	 *
	 * @return object Current row in result set
	 */
	public function current()
	{
		$this->check_fetched();
		return $this->_row;
	}

	/**
	 * Gets the number of the current row
	 *
	 * @return integer Row number
	 */
	public function key()
	{
		$this->check_fetched();
		return $this->_position;
	}

	/**
	 * Check if current row exists.
	 *
	 * @return bool True if row exists
	 */
	public function valid()
	{
		$this->check_fetched();
		return $this->_row != null;
	}

	/**
	 * Returns all rows as array
	 *
	 * @return array  Array of rows
	 */
	public function as_array()
	{
		$arr = array();
		foreach ($this as $row)
		{
			$arr[] = $row;
		}
		return $arr;
	}

	/**
	 * Checks if the rows from the result set have
	 * been fetched at least once. If not fetches first row.
	 *
	 */
	protected function check_fetched()
	{
		if (!$this->_fetched)
		{
			$this->_fetched = true;
			$this->next();
		}
	}

	/**
	 * Gets a column from the current row in the set
	 *
	 * @param  string $column Column name
	 * @return mixed  Column value
	 */
	public function get($column)
	{
		if ($this->valid() && isset($this->_row->$column))
		{
			return $this->_row->$column;
		}
	}

}