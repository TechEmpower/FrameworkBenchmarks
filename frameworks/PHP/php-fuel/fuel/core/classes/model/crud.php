<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Fuel\Core;

class Model_Crud extends \Model implements \Iterator, \ArrayAccess, \Serializable
{

	/**
	 * @var  string  $_table_name  The table name (must set this in your Model)
	 */
	// protected static $_table_name = '';

	/**
	 * @var  string  $_primary_key  The primary key for the table
	 */
	// protected static $_primary_key = 'id';

	/**
	 * @var string   $_connection   The database connection to use
	 */
	// protected static $_connection = null;

	/**
	 * @var string   $_write_connection   The database connection to use for writes
	 */
	// protected static $_write_connection = null;

	/**
	 * @var  array  $_rules  The validation rules (must set this in your Model to use)
	 */
	// protected static $_rules = array();

	/**
	 * @var  array  $_properties  The table column names (must set this in your Model to use)
	 */
	// protected static $_properties = array();

	/**
	 * @var array  $_labels  Field labels (must set this in your Model to use)
	 */
	// protected static $_labels = array();

	/**
	 * @var array  $_defaults  Field defaults (must set this in your Model to use)
	 */
	// protected static $_defaults = array();

	/**
	 * @var  bool  set true to use MySQL timestamp instead of UNIX timestamp
	 */
	//protected static $_mysql_timestamp = false;

	/**
	 * @var  string  fieldname of created_at field, uncomment to use.
	 */
	//protected static $_created_at = 'created_at';

	/**
	 * @var  string  fieldname of updated_at field, uncomment to use.
	 */
	//protected static $_updated_at = 'updated_at';

	/**
	 * Forges new Model_Crud objects.
	 *
	 * @param   array  $data  Model data
	 * @return  Model_Crud
	 */
	public static function forge(array $data = array())
	{
		return new static($data);
	}

	/**
	 * Finds a row with the given primary key value.
	 *
	 * @param   mixed  $value  The primary key value to find
	 * @return  null|object  Either null or a new Model object
	 */
	public static function find_by_pk($value)
	{
		return static::find_one_by(static::primary_key(), $value);
	}

	/**
	 * Finds a row with the given column value.
	 *
	 * @param   mixed  $column  The column to search
	 * @param   mixed  $value   The value to find
	 * @return  null|object  Either null or a new Model object
	 */
	public static function find_one_by($column, $value = null, $operator = '=')
	{
		$config = array(
			'limit' => 1,
		);

		if (is_array($column) or ($column instanceof \Closure))
		{
			$config['where'] = $column;
		}
		else
		{
			$config['where'] = array(array($column, $operator, $value));
		}

		$result = static::find($config);

		if ($result !== null)
		{
			return reset($result);
		}

		return null;
	}

	/**
	 * Finds all records where the given column matches the given value using
	 * the given operator ('=' by default).  Optionally limited and offset.
	 *
	 * @param   string  $column    The column to search
	 * @param   mixed   $value     The value to find
	 * @param   string  $operator  The operator to search with
	 * @param   int     $limit     Number of records to return
	 * @param   int     $offset    What record to start at
	 * @return  null|object  Null if not found or an array of Model object
	 */
	public static function find_by($column = null, $value = null, $operator = '=', $limit = null, $offset = 0)
	{
		$config = array(
			'limit' => $limit,
			'offset' => $offset,
		);

		if ($column !== null)
		{
			if (is_array($column) or ($column instanceof \Closure))
			{
				$config['where'] = $column;
			}
			else
			{
				$config['where'] = array(array($column, $operator, $value));
			}
		}

		return static::find($config);
	}

	/**
	 * Finds all records in the table.  Optionally limited and offset.
	 *
	 * @param   int     $limit     Number of records to return
	 * @param   int     $offset    What record to start at
	 * @return  null|object        Null if not found or an array of Model object
	 */
	public static function find_all($limit = null, $offset = 0)
	{
		return static::find(array(
			'limit' => $limit,
			'offset' => $offset,
		));
	}

	/**
	 * Finds all records.
	 *
	 * @param    array     $config     array containing query settings
	 * @param    string    $key        optional array index key
	 * @return   array|null            an array containing models or null if none are found
	 */
	public static function find($config = array(), $key = null)
	{
		$query = \DB::select()
			->from(static::$_table_name)
			->as_object(get_called_class());

		if ($config instanceof \Closure)
		{
			$config($query);
		}
		else
		{
			$config = $config + array(
				'select' => array(static::$_table_name.'.*'),
				'where' => array(),
				'order_by' => array(),
				'limit' => null,
				'offset' => 0,
			);

			extract($config);

			is_string($select) and $select = array($select);
			$query->select_array($select);

			if ( ! empty($where))
			{
				$query->where($where);
			}

			if (is_array($order_by))
			{
				foreach ($order_by as $_field => $_direction)
				{
					$query->order_by($_field, $_direction);
				}
			}
			else
			{
				$query->order_by($order_by);
			}

			if ($limit !== null)
			{
				$query = $query->limit($limit)->offset($offset);
			}
		}

		static::pre_find($query);

		$result =  $query->execute(static::get_connection());
		$result = ($result->count() === 0) ? null : $result->as_array($key);

		return static::post_find($result);
	}

	/**
	 * Count all of the rows in the table.
	 *
	 * @param   string  Column to count by
	 * @param   bool    Whether to count only distinct rows (by column)
	 * @param   array   Query where clause(s)
	 * @param   string  Column to group by
	 * @return  int     The number of rows OR false
	 */
	public static function count($column = null, $distinct = true, $where = array(), $group_by = null)
	{
		$select = $column ?: static::primary_key();

		// Get the database group / connection
		$connection = static::get_connection();

		// Get the columns
		$columns = \DB::expr('COUNT('.($distinct ? 'DISTINCT ' : '').
			\Database_Connection::instance($connection)->quote_identifier($select).
			') AS count_result');

		// Remove the current select and
		$query = \DB::select($columns);

		// Set from table
		$query = $query->from(static::$_table_name);

		if ( ! empty($where))
		{
			//is_array($where) or $where = array($where);
			if ( ! is_array($where) and ($where instanceof \Closure) === false)
			{
				throw new \FuelException(get_called_class().'::count where statement must be an array or a closure.');
			}
			$query = $query->where($where);
		}

		if ( ! empty($group_by))
		{
			$result = $query->select($group_by)->group_by($group_by)->execute($connection)->as_array();
			$counts = array();
			foreach ($result as $res)
			{
				$counts[$res[$group_by]] = $res['count_result'];
			}

			return $counts;
		}

		$count = $query->execute($connection)->get('count_result');

		if ($count === null)
		{
			return false;
		}

		return (int) $count;
	}

	/**
	 * Implements dynamic Model_Crud::find_by_{column} and Model_Crud::find_one_by_{column}
	 * methods.
	 *
	 * @param   string  $name  The method name
	 * @param   string  $args  The method args
	 * @return  mixed   Based on static::$return_type
	 * @throws  BadMethodCallException
	 */
	public static function __callStatic($name, $args)
	{
		if (strncmp($name, 'find_by_', 8) === 0)
		{
			return static::find_by(substr($name, 8), reset($args));
		}
		elseif (strncmp($name, 'find_one_by_', 12) === 0)
		{
			return static::find_one_by(substr($name, 12), reset($args));
		}
		throw new \BadMethodCallException('Method "'.$name.'" does not exist.');
	}

	/**
	 * Get the connection to use for reading or writing
	 *
	 * @param  boolean  $writeable Get a writeable connection
	 * @return Database_Connection
	 */
	protected static function get_connection($writeable = false)
	{
		if ($writeable and isset(static::$_write_connection))
		{
			return static::$_write_connection;
		}

		return isset(static::$_connection) ? static::$_connection : null;
	}

	/**
	 * Get the primary key for the current Model
	 *
	 * @return  string
	 */
	protected static function primary_key()
	{
		return isset(static::$_primary_key) ? static::$_primary_key : 'id';
	}

	/**
	 * Gets called before the query is executed.  Must return the query object.
	 *
	 * @param   Database_Query  $query  The query object
	 * @return  void
	 */
	protected static function pre_find(&$query){}

	/**
	 * Gets called after the query is executed and right before it is returned.
	 * $result will be null if 0 rows are returned.
	 *
	 * @param   array|null    $result    the result array or null when there was no result
	 * @return  array|null
	 */
	protected static function post_find($result)
	{
		return $result;
	}

	/**
	 * @var  bool  $_is_new  If this is a new record
	 */
	protected $_is_new = true;

	/**
	 * @var  bool  $_is_frozen  If this is a record is frozen
	 */
	protected $_is_frozen = false;

	/**
	 * @var  object  $_validation  The validation instance
	 */
	protected $_validation = null;

	/**
	 * Sets up the object.
	 *
	 * @param   array  $data  The data array
	 * @return  void
	 */
	public function __construct(array $data = array())
	{
		if (isset($this->{static::primary_key()}))
		{
			$this->is_new(false);
		}

		if ( ! empty($data))
		{
			foreach ($data as $key => $value)
			{
				$this->{$key} = $value;
			}
		}
	}

	/**
	 * Magic setter so new objects can be assigned values
	 *
	 * @param   string  $property  The property name
	 * @param   mixed   $value     The property value
	 * @return  void
	 */
	public function __set($property, $value)
	{
		$this->{$property} = $value;
	}

	/**
	 * Sets an array of values to class properties
	 *
	 * @param   array  $data  The data
	 * @return  $this
	 */
	public function set(array $data)
	{
		foreach ($data as $key => $value)
		{
			$this->{$key} = $value;
		}
		return $this;
	}

	/**
	 * Saves the object to the database by either creating a new record
	 * or updating an existing record. Sets the default values if set.
	 *
	 * @param   bool   $validate  wether to validate the input
	 * @return  mixed  Rows affected and or insert ID
	 */
	public function save($validate = true)
	{
		if ($this->frozen())
		{
			throw new \Exception('Cannot modify a frozen row.');
		}

		$vars = $this->to_array();

		// Set default if there are any
		isset(static::$_defaults) and $vars = $vars + static::$_defaults;

		if ($validate and isset(static::$_rules) and ! empty(static::$_rules))
		{
			$vars = $this->pre_validate($vars);
			$validated = $this->post_validate($this->run_validation($vars));

			if ($validated)
			{
				$validated = array_filter($this->validation()->validated(), function($val){
					return ($val !== null);
				});

				$vars = $validated + $vars;
			}
			else
			{
				return false;
			}
		}

		$vars = $this->prep_values($vars);

		if (isset(static::$_properties))
		{
			$vars = \Arr::filter_keys($vars, static::$_properties);
		}

		if(isset(static::$_updated_at))
		{
			if(isset(static::$_mysql_timestamp) and static::$_mysql_timestamp === true)
			{
				$vars[static::$_updated_at] = \Date::forge()->format('mysql');
			}
			else
			{
				$vars[static::$_updated_at] = \Date::forge()->get_timestamp();
			}
		}

		if ($this->is_new())
		{
			if(isset(static::$_created_at))
			{
				if(isset(static::$_mysql_timestamp) and static::$_mysql_timestamp === true)
				{
					$vars[static::$_created_at] = \Date::forge()->format('mysql');
				}
				else
				{
					$vars[static::$_created_at] = \Date::forge()->get_timestamp();
				}
			}

			$query = \DB::insert(static::$_table_name)
			            ->set($vars);

			$this->pre_save($query);
			$result = $query->execute(static::get_connection(true));

			if ($result[1] > 0)
			{
				// workaround for PDO connections not returning the insert_id
				if ($result[0] === false and isset($vars[static::primary_key()]))
				{
					$result[0] = $vars[static::primary_key()];
				}
				$this->set($vars);
				empty($result[0]) or $this->{static::primary_key()} = $result[0];
				$this->is_new(false);
			}

			return $this->post_save($result);
		}

		$query = \DB::update(static::$_table_name)
		         ->set($vars)
		         ->where(static::primary_key(), '=', $this->{static::primary_key()});

		$this->pre_update($query);
		$result = $query->execute(static::get_connection(true));
		$result > 0 and $this->set($vars);

		return $this->post_update($result);
	}

	/**
	 * Deletes this record and freezes the object
	 *
	 * @return  mixed  Rows affected
	 */
	public function delete()
	{
		$this->frozen(true);
		$query = \DB::delete(static::$_table_name)
		            ->where(static::primary_key(), '=', $this->{static::primary_key()});

		$this->pre_delete($query);
		$result = $query->execute(static::get_connection(true));

		return $this->post_delete($result);
	}

	/**
	 * Either checks if the record is new or sets whether it is new or not.
	 *
	 * @param   bool|null  $new  Whether this is a new record
	 * @return  bool|$this
	 */
	public function is_new($new = null)
	{
		if ($new === null)
		{
			return $this->_is_new;
		}

		$this->_is_new = (bool) $new;

		return $this;
	}

	/**
	 * Either checks if the record is frozen or sets whether it is frozen or not.
	 *
	 * @param   bool|null  $new  Whether this is a frozen record
	 * @return  bool|$this
	 */
	public function frozen($frozen = null)
	{
		if ($frozen === null)
		{
			return $this->_is_frozen;
		}

		$this->_is_frozen = (bool) $frozen;

		return $this;
	}

	/**
	 * Returns the a validation object for the model.
	 *
	 * @return  object  Validation object
	 */
	public function validation()
	{
		if( ! $this->_validation)
		{
			$this->_validation = \Validation::forge(\Str::random('alnum', 32));

			if (isset(static::$_rules) and count(static::$_rules))
			{
				foreach (static::$_rules as $field => $rules)
				{
					$label = (isset(static::$_labels) and array_key_exists($field, static::$_labels)) ? static::$_labels[$field] : $field;
					$this->_validation->add_field($field, $label, $rules);
				}
			}
		}

		return $this->_validation;
	}

	/**
	 * Returns all of $this object's public properties as an associative array.
	 *
	 * @return  array
	 */
	public function to_array()
	{
		return get_object_public_vars($this);
	}

	/**
	 * Implementation of the Iterator interface
	 */

	protected $_iterable = array();

	public function rewind()
	{
		$this->_iterable = $this->to_array();
		reset($this->_iterable);
	}

	public function current()
	{
		return current($this->_iterable);
	}

	public function key()
	{
		return key($this->_iterable);
	}

	public function next()
	{
		return next($this->_iterable);
	}

	public function valid()
	{
		return key($this->_iterable) !== null;
	}

	/**
	 * Sets the value of the given offset (class property).
	 *
	 * @param   string  $offset  class property
	 * @param   string  $value   value
	 * @return  void
	 */
	public function offsetSet($offset, $value)
	{
		$this->{$offset} = $value;
	}

	/**
	 * Checks if the given offset (class property) exists.
	 *
	 * @param   string  $offset  class property
	 * @return  bool
	 */
	public function offsetExists($offset)
	{
		return property_exists($this, $offset);
	}

	/**
	 * Unsets the given offset (class property).
	 *
	 * @param   string  $offset  class property
	 * @return  void
	 */
	public function offsetUnset($offset)
	{
		unset($this->{$offset});
	}

	/**
	 * Gets the value of the given offset (class property).
	 *
	 * @param   string  $offset  class property
	 * @return  mixed
	 */
	public function offsetGet($offset)
	{
		if (property_exists($this, $offset))
		{
			return $this->{$offset};
		}

		throw new \OutOfBoundsException('Property "'.$offset.'" not found for '.get_called_class().'.');
	}

	/**
	 * Returns wether the instance will pass validation.
	 *
	 * @return  bool  wether the instance passed validation
	 */
	public function validates()
	{
		if ( ! isset(static::$_rules) or count(static::$_rules) < 0)
		{
			return true;
		}

		$vars = $this->to_array();

		// Set default if there are any
		isset(static::$_defaults) and $vars = $vars + static::$_defaults;
		$vars = $this->pre_validate($vars);

		return $this->run_validation($vars);
	}

	/**
	 * Run validation
	 *
	 * @param   array  $vars  array to validate
	 * @return  bool   validation result
	 */
	protected function run_validation($vars)
	{
		if ( ! isset(static::$_rules) or count(static::$_rules) < 0)
		{
			return true;
		}

		$this->_validation = $this->validation();

		return $this->_validation->run($vars);
	}

	/**
	 * Gets called before the insert query is executed.  Must return
	 * the query object.
	 *
	 * @param   Database_Query  $query  The query object
	 * @return  void
	 */
	protected function pre_save(&$query){}

	/**
	 * Gets called after the insert query is executed and right before
	 * it is returned.
	 *
	 * @param   array  $result  insert id and number of affected rows
	 * @return  array
	 */
	protected function post_save($result)
	{
		return $result;
	}

	/**
	 * Gets called before the update query is executed.  Must return the query object.
	 *
	 * @param   Database_Query  $query  The query object
	 * @return  void
	 */
	protected function pre_update(&$query){}

	/**
	 * Gets called after the update query is executed and right before
	 * it is returned.
	 *
	 * @param   int  $result  Number of affected rows
	 * @return  int
	 */
	protected function post_update($result)
	{
		return $result;
	}

	/**
	 * Gets called before the delete query is executed.  Must return the query object.
	 *
	 * @param   Database_Query  $query  The query object
	 * @return  void
	 */
	protected function pre_delete(&$query){}

	/**
	 * Gets called after the delete query is executed and right before
	 * it is returned.
	 *
	 * @param   int  $result  Number of affected rows
	 * @return  int
	 */
	protected function post_delete($result)
	{
		return $result;
	}

	/**
	 * Gets called before the validation is ran.
	 *
	 * @param   array  $data  The validation data
	 * @return  array
	 */
	protected function pre_validate($data)
	{
		return $data;
	}

	/**
	 * Called right after the validation is ran.
	 *
	 * @param   bool  $result  Validation result
	 * @return  bool
	 */
	protected function post_validate($result)
	{
		return $result;
	}

	/**
	 * Called right after values retrieval, before save,
	 * update, setting defaults and validation.
	 *
	 * @param   array  $values  input array
	 * @return  array
	 */
	protected function prep_values($values)
	{
		return $values;
	}

	/**
	 * Serializable implementation: serialize
	 *
	 * @return  array  model data
	 */
	public function serialize()
	{
		$data = $this->to_array();

		$data['_is_new'] = $this->_is_new;
		$data['_is_frozen'] = $this->_is_frozen;

		return serialize($data);
	}

	/**
	 * Serializable implementation: unserialize
	 *
	 * @return  array  model data
	 */
	public function unserialize($data)
	{
		$data = unserialize($data);

		foreach ($data as $key => $value)
		{
			$this->__set($key, $value);
		}
	}
}
