<?php

namespace PHPixie\DB\Mysql;

/**
 * Database result implementation for Mysqli
 * @package Database
 */
class Result extends \PHPixie\DB\Result
{

	/**
	 * Initializes new result object
	 *
	 * @param mysqli_result $result Mysqli Result
	 * @return void
	 * @link http://php.net/manual/en/class.mysqli-result.php
	 */
	public function __construct($result)
	{
		$this->_result = $result;
	}

	/**
	 * Throws exception if rewind is attempted.
	 *
	 * @return void
	 * @throws \Exception If rewind is attempted
	 */
	public function rewind()
	{
		if ($this->_position > 0)
		{
			throw new \Exception('Mysqli result cannot be rewound for unbuffered queries.');
		}
	}

	/**
	 * Iterates to the next row in the result set
	 *
	 * @return void
	 */
	public function next()
	{
		$this->check_fetched();
		$this->_row = $this->_result->fetch_object();
		if ($this->_row)
		{
			$this->_position++;
		}
		else
		{
			$this->_result->free();
		}
	}

}