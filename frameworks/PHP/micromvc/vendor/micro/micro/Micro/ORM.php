<?php
/**
 * ORM (Object-relational mapping)
 *
 * Allows the application to work directly with data in the database by modeling
 * it as native PHP objects. In other words, no more SQL queries. This ORM class
 * uses Index-Only SQL to make the most of object cacheing. It is advised you
 * use APC, Memcached, or another RAM cache along with this class.
 *
 * When creating your models you must use the following public variables to
 * define relations among your objects.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

class ORM
{

	// object data, related, changed, loaded, saved
	public $data, $related, $changed, $loaded, $saved;

	public static $db;
	public static $table;
	public static $key = 'id';
	public static $foreign_key;
	public static $belongs_to;
	public static $has;					// Has one/many
	public static $has_many_through;	// Has many through
	public static $order_by;
	public static $cache = 0;
	public static $cascade_delete = FALSE;


	/**
	 * Create a new database entity object
	 *
	 * @param int|mixed $id of the row or row object
	 */
	public function __construct($id = 0)
	{
		$this->data = array();

		if(! $id) return;

		if(is_numeric($id))
		{
			$this->data[static::$key] = $id;
		}
		else
		{
			$this->data = (array) $id;
			$this->loaded = 1;
		}

		$this->saved  = 1;
	}


	/**
	 * Get this object's primary key
	 *
	 * @return int
	 */
	public function key()
	{
		return isset($this->data[static::$key])?$this->data[static::$key]:NULL;
	}


	/**
	 * Return object data as array
	 *
	 * @return array
	 */
	public function to_array()
	{
		if($this->load()) return $this->data;
	}


	/**
	 * Set an array of values on this object
	 *
	 * @param array $values to set
	 * @return object
	 */
	public function set($values)
	{
		foreach($values as $key => $value)
		{
			$this->__set($key, $value);
		}
		return $this;
	}


	/**
	 * Set a propery of this object
	 *
	 * @param string $key name
	 * @param mixed $v value
	 */
	public function __set($key, $value)
	{
		if( ! array_key_exists($key, $this->data) OR $this->data[$key] !== $value)
		{
			$this->data[$key] = $value;
			$this->changed[$key] = $key;
			$this->saved = 0;
		}
	}


	/**
	 * Retive a property or 1-to-1 object relation
	 *
	 * @param string $key the column or relation name
	 * @return mixed
	 */
	public function __get($key)
	{
		// All this to get the primary key without loading the entity
		if(isset($this->data[static::$key]))
		{
			if($key == static::$key) return $this->data[static::$key];
			if( ! $this->loaded) $this->load();
		}

		//if(isset($this->data[static::$key]) AND ! $this->loaded) $this->load();
		return array_key_exists($key, $this->data) ? $this->data[$key] : $this->related($key);
	}


	/**
	 * @see isset()
	 */
	public function __isset($key)
	{
		if(isset($this->data[static::$key]) AND ! $this->loaded) $this->load();
		return array_key_exists($key, $this->data) OR isset($this->related[$key]);
	}


	/**
	 * @see unset()
	 */
	public function __unset($key)
	{
		unset($this->data[$key], $this->changed[$key], $this->related[$key]);
	}


	/**
	 * Reload the current object from the database
	 *
	 * @return boolean
	 */
	public function reload()
	{
		$key = $this->key();
		$this->data = $this->changed = $this->related = array();
		$this->loaded = FALSE;
		if(! $key) return;
		$this->data[static::$key] = $key;
		return $this->load();
	}


	/**
	 * Clear the current object
	 */
	public function clear()
	{
		$this->data = $this->changed = $this->related = array();
		$this->loaded = $this->saved = FALSE;
	}


	/**
	 * Attempt to load the object record from the database
	 *
	 * @return boolean
	 */
	public function load(array $where = NULL)
	{
		$key = static::$key;

		if($where)
		{
			// Find the record primary key in the database
			$id = self::select('column', static::$key, NULL, $where);

			if(empty($id))
			{
				$this->clear();
				return FALSE;
			}

			$this->data[$key] = $id;
		}
		else
		{
			// Did we already load this object?
			if($this->loaded) return TRUE;

			if(empty($this->data[$key]))
			{
				//$this->clear();
				return FALSE;
			}

			// Use the record primary key given in constructor
			$id = $this->data[$key];
		}


		// First check the cache
		if(!($row = static::cache_get(static::$table . $id)))
		{
			// Then get from the database and cache
			if($row = self::select('row', '*', $this, array($key => $id)))
			{
				static::cache_set(static::$table . $id, $row);
			}
		}

		if($row)
		{
			$this->data = (array) $row;
			return $this->saved = $this->loaded = TRUE;
		}
		else
		{
			$this->clear();
		}
	}


	/**
	 * Load a related 1-to-1 object
	 *
	 * @param string $alias relation alias
	 * @return object
	 */
	public function related($alias)
	{
		// Already loaded?
		if(isset($this->related[$alias])) return $this->related[$alias];

		if(isset(static::$belongs_to[$alias]))
		{
			$model = static::$belongs_to[$alias];

			if(is_array($model))
			{
				$foreign_key = key($model);
				$model = current($model);
			}
			else
			{
				$foreign_key = $model::$foreign_key;
			}

			return $this->related[$alias] = new $model($this->data[$foreign_key]);
		}
		elseif(isset(static::$has[$alias]))
		{
			$model = static::$has[$alias];

			if(is_array($model))
			{
				$foreign_key = key($model);
				$model = current($model);
			}
			else
			{
				$foreign_key = static::$foreign_key;
			}

			// Fetch the ID of the models row
			$id = self::select('column', $model::$key, $model, array($foreign_key => $this->key()));

			return $this->related[$alias] = new $model($id);
		}
		else
		{
			throw new \Exception(get_class($this). " propery $alias not found");
		}
	}


	/**
	 * Load a has_many relation set from another model using the filtering options of fetch()
	 *
	 * @param string $m alias name
	 * @param mixed $a arguments to pass
	 * @return array
	 */
	public function __call($alias, $args)
	{
		$method = 'fetch';

		if(substr($alias, 0, 6) === 'count_')
		{
			$method = 'count';
			$alias = substr($alias, 6);
		}

		// Append the default filter options
		$args = $args + array(array(), 0, 0, array());


		// Is this a has one/many relation?
		if(isset(static::$has[$alias]))
		{
			$model = static::$has[$alias];

			if(is_array($model))
			{
				$foreign_key = key($model);
				$model = current($model);
			}
			else
			{
				$foreign_key = static::$foreign_key;
			}

			// Set the foreign key WHERE condition
			$args[0][$foreign_key] = $this->key();

			return $model::$method($args[0], $args[1], $args[2], $args[3]);
		}

		if(empty(static::$has_many_through[$alias]))
		{
			throw new \Exception ($alias . ' relation not found');
		}

		$array = static::$has_many_through[$alias];

		$foreign_key = key($array);
		$model = current($array);

		next($array);

		$foreign_key_2 = key($array);
		$model_2 = current($array);

		// Set the foreign key WHERE condition
		$where = array($foreign_key => $this->key()) + $args[0];

		// Fetch an array of objects by the foreign key so we can load from memory
		return self::objects($foreign_key_2, $model_2, $model, $where, $args[1], $args[2], $args[3]);
	}


	/**
	 * Load an array of objects from the database
	 *
	 * @param string $column column to load
	 * @param object $class class to load into
	 * @param object $model model to search
	 * @param array $where where conditions
	 * @param int $limit limit
	 * @param int $offset offset
	 * @param array $order by conditions
	 * @return array
	 */
	public static function objects($column = NULL, $class = NULL, $model = NULL, $where = NULL, $limit = 0, $offset = 0, $order = NULL)
	{
		if($rows = self::select('fetch', $column, $model, $where, $limit, $offset, $order))
		{
			$class = $class ?: get_called_class();
			foreach($rows as $id => $row)
			{
				$rows[$id] = new $class($row);
			}
		}
		return $rows;
	}


	/**
	 * Load a SELECT query result set
	 *
	 * @param string $func function name (column/row/fetch)
	 * @param string $column column(s) to fetch
	 * @param object $model model to search
	 * @param array $where where conditions
	 * @param int $limit limit
	 * @param int $offset
	 * @param array $order by conditions
	 * @return mixed
	 */
	public static function select($func, $column, $model = NULL, $where = NULL, $limit = 0, $offset = 0, $order = NULL)
	{
		$model = $model ?: get_called_class();
		$order = ($order ?: array()) + (static::$order_by ?: array());

		// Count queries don't have offsets, limits, or order conditions
		if($func != 'fetch')
		{
			$limit = $offset = 0;
			$order = array();
		}

		// Generate select statement SQL
		list($sql, $params) = static::$db->select(($column ? $column : 'COUNT(*)'), $model::$table, $where, $limit, $offset, $order);

		return static::$db->$func($sql, $params, ($column == '*' ? NULL : 0));
	}


	/**
	 * Fetch an array of objects from this table
	 *
	 * @param array $where conditions
	 * @param int $limit filter
	 * @param int $offset filter
	 * @param array $order_by conditions
	 */
	public static function fetch(array $where = NULL, $limit = 0, $offset = 0, array $order_by = NULL)
	{
		return self::objects(static::$key, 0, 0, $where, $limit, $offset, $order_by);
	}


	/**
	 * Count all database rows matching the conditions
	 *
	 * @param array $where conditions
	 * @return int
	 */
	public static function count(array $where = NULL)
	{
		return self::select('column', NULL, NULL, $where);
	}


	/**
	 * Return the result column of the row that matches the where condition.
	 * This can be used to get a rows primary key.
	 *
	 * @param array $where conditions
	 * @return int
	 */
	public static function column(array $where = NULL, $column = NULL)
	{
		return self::select('column', $column ? $column : static::$key, NULL, $where);
	}


	/**
	 * Return the ORM object which matches the where condition
	 *
	 * @param array $where conditions
	 * @return int
	 */
	public static function row(array $where = NULL)
	{
		if($id = self::select('column', static::$key, NULL, $where))
		{
			$class = get_called_class();
			return new $class($id);
		}
	}


	/**
	 * Save the current object to the database
	 */
	public function save()
	{
		if( ! $this->changed) return $this;

		$data = array();
		foreach($this->changed as $column)
		{
			$data[$column] = $this->data[$column];
		}

		if(isset($this->data[static::$key]))
		{
			$this->update($data);
		}
		else
		{
			$this->insert($data);
		}

		$this->changed = array();
		return $this;
	}


	/**
	 * Insert the current object into the database table
	 *
	 * @param array $data to insert
	 * @return int
	 */
	protected function insert(array $data)
	{
		$id = static::$db->insert(static::$table, $data);

		$this->data[static::$key] = $id;
		$this->loaded = $this->saved = 1;
		return $id;
	}


	/**
	 * Update the current object in the database table
	 *
	 * @param array $d data
	 * @return boolean
	 */
	protected function update(array $data)
	{
		$result = static::$db->update(static::$table, $data, array(static::$key => $this->data[static::$key]));

		// Invalidate cache
		static::cache_delete(static::$table . $this->data[static::$key]);

		$this->saved = 1;
		return $result;
	}


	/**
	 * Delete the current object (and all related objects) from the database
	 *
	 * @param int $id to delete
	 * @return int
	 */
	public function delete($id = NULL)
	{
		$id = $id ?: $this->key();

		$count = 0;

		// Remove all related entities too?
		if(static::$cascade_delete)
		{
			$count = $this->delete_relations();
		}

		$table = static::$db->i . static::$table . static::$db->i;

		// Then remove this entity
		$count += static::$db->delete('DELETE FROM ' . $table . ' WHERE ' . static::$key . ' = ?', array($id));

		// Remove remaining traces
		static::cache_delete(static::$table . $id);
		$this->clear();

		return $count;
	}


	/**
	 * Delete all the related objects that belong to the current object
	 *
	 * @return int
	 */
	public function delete_relations()
	{
		$count = 0;
		foreach(static::$has as $alias => $model)
		{
			foreach($this->$alias() as $object)
			{
				// This object may also have entities to remove first
				$count += $object->delete();
			}
		}
		return $count;
	}


	/**
	 * Store a value in the cache
	 *
	 * @param string $key name
	 * @param mixed $value to store
	 */
	public static function cache_set($key, $value){}


	/**
	 * Fetch a value from the cache
	 *
	 * @param string $key name
	 * @return mixed
	 */
	public static function cache_get($key){}


	/**
	 * Delete a value from the cache
	 *
	 * @param string $key name
	 * @return boolean
	 */
	public static function cache_delete($key){}


	/**
	 * Check that a value exists in the cache
	 *
	 * @param string $key name
	 * @return boolean
	 */
	public static function cache_exists($key){}

}

// END
