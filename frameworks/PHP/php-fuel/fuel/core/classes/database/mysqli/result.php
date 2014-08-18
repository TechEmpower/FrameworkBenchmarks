<?php
/**
 * MySQL database result.
 *
 * @package    Fuel/Database
 * @category   Query/Result
 * @author     Kohana Team
 * @copyright  (c) 2008-2009 Kohana Team
 * @license    http://kohanaphp.com/license
 */

namespace Fuel\Core;

class Database_MySQLi_Result extends \Database_Result
{

	protected $_internal_row = 0;

	public function __construct($result, $sql, $as_object)
	{
		parent::__construct($result, $sql, $as_object);

		// Find the number of rows in the result
		$this->_total_rows = $result->num_rows;
	}

	public function __destruct()
	{
		if ($this->_result instanceof \MySQLi_Result)
		{
			$this->_result->free();
		}
	}

	public function seek($offset)
	{
		if ($this->offsetExists($offset) and $this->_result->data_seek($offset))
		{
			// Set the current row to the offset
			$this->_current_row = $this->_internal_row = $offset;

			return true;
		}
		else
		{
			return false;
		}
	}

	public function current()
	{
		if ($this->_current_row !== $this->_internal_row and ! $this->seek($this->_current_row))
			return false;

		// Increment internal row for optimization assuming rows are fetched in order
		$this->_internal_row++;

		if ($this->_as_object === true)
		{
			// Return an stdClass
			return $this->_result->fetch_object();
		}
		elseif (is_string($this->_as_object))
		{
			// Return an object of given class name
			//! TODO: add the $params parameter
			return $this->_result->fetch_object($this->_as_object);
		}
		else
		{
			// Return an array of the row
			return $this->_result->fetch_assoc();
		}
	}

}
