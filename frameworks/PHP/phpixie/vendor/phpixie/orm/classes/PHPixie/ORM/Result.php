<?php

namespace PHPixie\ORM;

/**
 * Allows iterating over ORM objects inside loops lie 'foreach',
 * while preserving performance by working with only a single row
 * at a time. It wraps conveniently wraps around Database_Result class
 * returning ORM object instead of just data object.
 *
 * @see \PHPixie\Database\Result
 * @package ORM
 */
class Result implements \Iterator
{

	/**
	 * Pixie Dependancy Container
	 * @var \PHPixie\Pixie
	 */
	public $pixie;
	
	/**
	 * Name of the model that the rows belong to
	 * @var string
	 */
	protected $_model;

	/**
	 * Database result
	 * @var Result_Database
	 */
	protected $_dbresult;

	/**
	 * Rules for preloaded relationships
	 * @var array
	 */
	protected $_with = array();

	/**
	 * Initialized an Result_ORM with which model to use and which result to
	 * iterate over
	 *
	 * @param string          $model  Model name
	 * @param Result_Database $dbresult Database result
	 * @param array           $with Array of rules for preloaded relationships
	 * @return void
	 */
	public function __construct($pixie, $model, $dbresult, $with = array())
	{
		$this->pixie = $pixie;
		$this->_model = $model;
		$this->_dbresult = $dbresult;
		foreach ($with as $path => $rel)
		{
			$this->_with[] = array(
				'path' => explode('.', $path),
				'path_count' => count(explode('.', $path)),
				'model' => $rel['model'],
				'columns' => $rel['model']->columns(),
			);
		}
	}

	/**
	 * Rewinds database cursor to the first row
	 *
	 * @return void
	 */
	function rewind()
	{
		$this->_dbresult->rewind();
	}
	
	/**
	 * Builds a model instance based on row data.
	 *
	 * @param array $data Item data
	 * @return \PHPixie\ORM\Model Model instance initialized with item data
	 */
	public function build_model($data) {
		
		$model = $this->pixie->orm->get($this->_model);

		if (empty($data))
		{
			return $model;
		}

		if (empty($this->_with))
		{
			return $model->values($data, true);
		}


		$model_data = array();
		foreach ($model->columns() as $column)
		{
			$model_data[$column] = array_shift($data);
		}
		$model->values($model_data, true);

		foreach ($this->_with as $rel)
		{
			$rel_data = array();
			foreach ($rel['columns'] as $column)
			{
				$rel_data[$column] = array_shift($data);
			}
			$rel['model']->values($rel_data, true);

			$owner = $model;
			foreach ($rel['path'] as $key => $child)
			{
				if ($key == $rel['path_count'] - 1)
				{
					$owner->cached[$child] = $rel['model'];
				}
				else
				{
					$owner = $owner->cached[$child];
				}
			}
		}

		return $model;	
		
	}
	
	/**
	 * Gets an ORM Model of the current row
	 *
	 * @return \PHPixie\ORM\Model Model of the current row of the result set
	 */
	public function current()
	{
		$data = $this->_dbresult->valid()
			?((array) $this->_dbresult->current())
			:null;
			
		return $this->build_model($data);
	}

	/**
	 * Gets current rows' index number
	 *
	 * @return int Row number
	 */
	function key()
	{
		return $this->_dbresult->key();
	}

	/**
	 * Iterates to the next row in the result
	 *
	 * @return void
	 */
	function next()
	{
		$this->_dbresult->next();
	}

	/**
	 * Checks if current row is valid.
	 *
	 * @return bool returns false if we reach the end of the result set.
	 */
	function valid()
	{
		return $this->_dbresult->valid();
	}

	/**
	 * Returns an array of all rows as ORM objects if $rows is False,
	 * or just an array of result rows with each row being a standard object,
	 * this can be useful for functions like json_encode.
	 *
	 * @param boolean $rows Whether to return just rows and not ORM objects
	 * @return array   Array of ORM objects or standard objects representing rows
	 */
	public function as_array($rows = false)
	{
		if (!$rows)
		{
			$arr = array();
			foreach ($this as $row)
				$arr[] = $row;
			return $arr;
		}

		if (empty($this->_with))
		{
			return $this->_dbresult->as_array();
		}

		$arr = array();
		$model = $model = $this->pixie->orm->get($this->_model);
		foreach ($this->_dbresult as $data)
		{
			$row = new \stdClass;
			$data = (array) $data;
			foreach ($model->columns() as $column)
			{
				$row->$column = array_shift($data);
			}

			foreach ($this->_with as $rel)
			{
				$rel_data = new \StdClass;
				foreach ($rel['columns'] as $column)
				{
					$rel_data->$column = array_shift($data);
				}

				$owner = &$row;
				foreach ($rel['path'] as $key => $child)
				{
					if ($key == $rel['path_count'] - 1)
					{
						$owner->$child = $rel_data;
					}
					else
					{
						$owner = &$owner->$child;
					}
				}
			}
			$arr[] = $row;
		}

		return $arr;
	}

}
