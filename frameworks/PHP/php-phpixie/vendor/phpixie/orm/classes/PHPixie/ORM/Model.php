<?php

namespace PHPixie\ORM;

/**
 * ORM allows you to access database items and their relationships in an OOP manner,
 * it is easy to setup and makes a lot of use of naming convention.
 *
 * @method mixed limit(int $limit = null) Set number of rows to return.
 *               If NULL is passed than no limit is used.
 *               Without arguments returns current limit, returns self otherwise.
 *
 * @method mixed offset(int $offset = null) Set the offset for the first row in result.
 *               If NULL is passed than no limit is used.
 *               Without arguments returns current offset, returns self otherwise.
 *
 * @method mixed order_by(string $column, string $dir) Adds a column to ordering parameters
 *
 * @method mixed where(mixed $key, mixed $operator = null, mixed $val = null) behaves just like Query_Database::where()
 *
 * @see Query_Database::where()
 * @package ORM
 */
class Model
{

	/**
	 * Pixie Dependancy Container
	 * @var \PHPixie\Pixie
	 */
	public $pixie;

	/**
	 * Database Connection
	 * @var \PHPixie\DB\Connection
	 */
	public $conn;
	
	/**
	 * Specifies which table the model will use, can be overridden
	 * @var string
	 */
	public $table = null;

	/**
	 * Specifies which connection the model will use, can be overridden
	 * but a model can have relationships only with models utilizing the same connection
	 * @var string
	 */
	public $connection = 'default';

	/**
	 * Specifies which column is treated as PRIMARY KEY
	 * @var string
	 */
	public $id_field = 'id';

	/**
	 * You can define 'Belongs to' relationships buy changing this array
	 * @var array
	 */
	protected $belongs_to = array();

	/**
	 * You can define 'Has one' relationships buy changing this array
	 * @var array
	 */
	protected $has_one = array();

	/**
	 * You can define 'Has many' relationships buy changing this array
	 * @var array
	 */
	protected $has_many = array();

	/**
	 * Associated query builder
	 * @var \PHPixie\DB\Query
	 */
	public $query;

	/**
	 * The name of the model
	 * @var string
	 */
	public $model_name;

	/**
	 * Cached properties
	 * @var array
	 */
	public $cached = array();

	/**
	 * An instance of the database connection
	 * @var DB
	 */
	protected $db;

	/**
	 * Current row returned by the database
	 * @var array
	 */
	protected $_row = array();

	/**
	 * A flag whether the row was loaded from the database
	 * @var boolean
	 */
	protected $_loaded = false;

	/**
	 * Relationships to be preloaded
	 * @var array
	 */
	protected $_with = array();
	
	/**
	 * Extension definitions
	 * @var array
	 */
	protected $extensions = array();
	
	/**
	 * Extension instances
	 * @var array
	 */
	protected $extension_instances = array();
	
	/**
	 * Cached column names for tables
	 * @var array
	 */
	protected static $_column_cache = array();

	/**
	 * Constructs the model. To use ORM it is enough to
	 * just create a model like this:
	 * <code>
	 * class App\Model\Fairy extends \PHPixie\ORM\Model { }
	 * </code>
	 * By default it will assume that the name of your table
	 * is the plural form of the models' name, the PRIMARY KEY is id,
	 * and will use the 'default' connection. This behaviour is easy to be
	 * changed by overriding $table, $id and $db properties.
	 *
	 * @param \PHPixie\Pixie $pixie Pixie dependency container
	 * @return void
	 * @see $table
	 * @see $id
	 * @see $db
	 */
	public function __construct($pixie)
	{
		$this->pixie = $pixie;
		$this->conn = $pixie->db->get($this->connection);
		$this->query = $this->conn->query('select');
		$this->model_name = strtolower(get_class($this));
		$this->model_name = str_ireplace($this->pixie->app_namespace."Model\\", '', $this->model_name);
		
		if ($this->table == null)
		{
			$this->table = str_replace("\\", '_', $this->model_name);
			$this->table = $this->plural($this->table);
		}
		$this->query->table($this->table);

		foreach (array('belongs_to', 'has_one', 'has_many') as $rels)
		{
			$normalized = array();
			foreach ($this->$rels as $key => $rel)
			{
				if (!is_array($rel))
				{
					$key = $rel;
					$rel = array();
				}
				$normalized[$key] = $rel;
				if (!isset($rel['model']))
				{
					$rel['model'] = $normalized[$key]['model'] = $rels == 'has_many' ? $this->singular($key) : $key;
				}

				$normalized[$key]['type'] = $rels;
				if (!isset($rel['key']))
				{
					$normalized[$key]['key'] = $this->model_key( $rels != 'belongs_to' ? $this->model_name : $rel['model']);
				}

				if ($rels == 'has_many' && isset($rel['through']))
				{
					if (!isset($rel['foreign_key']))
					{
						$normalized[$key]['foreign_key'] = $this->model_key($rel['model']);
					}
				}

				$normalized[$key]['name'] = $key;
			}
			$this->$rels = $normalized;
		}
	}
	
	/**
	 * Get model foreign key from model name
	 *
	 * @param string $model_name      Name of the model
	 *
	 * @return string  Foreign key for specified model name.
	 */
	public function model_key($model_name) {
		return str_replace("\\", '_', $model_name).'_id';
	}
	
	/**
	 * Magic method for call Query_Database methods
	 *
	 * @param string $method      Method to call
	 * @param array $arguments Arguments passed to the method
	 * @return mixed  Returns self if parameters were passed. If no parameters where passed returns
	 *                current value for the associated parameter
	 * @throws \Exception If method doesn't exist
	 */
	public function __call($method, $arguments)
	{
		if (!in_array($method, array('limit', 'offset', 'order_by', 'where')))
		{
			throw new \Exception("Method '{$method}' doesn't exist on .".get_class($this));
		}
		$res = call_user_func_array(array($this->query, $method), $arguments);
		if (empty($arguments))
			return $res;
		
		return $this;
	}
	
	/**
	 * Prepares the relationships specified using with().
	 *
	 * @return array Relationship definitions used by \PHPixie\ORM\Result
	 * @throw  \Exception If the relationship specified using with() does not exist or is not of the belongs_to or has_one type
	 */
	public function prepare_relations() {
		$paths = array();
		if (!empty($this->_with))
		{
			$fields = array();
			$this_alias = $this->query->last_alias();
			foreach ($this->columns() as $column)
			{
				$fields[] = array("{$this_alias}.{$column}", "{$this_alias}__{$column}");
			}
			foreach ($this->_with as $target)
			{
				$model = $this;
				$model_alias = $this_alias;
				$rels = explode('.', $target);
				foreach ($rels as $key => $rel_name)
				{
					$path = implode('.', array_slice($rels, 0, $key + 1));
					if (isset($paths[$path]))
					{
						$model = $paths[$path]['model'];
						$model_alias = $paths[$path]['alias'];
						continue;
					}
					$alias = $this->query->add_alias();
					$model_rels = array_merge($model->has_one, $model->has_many, $model->belongs_to);
					$rel = $this->pixie->arr($model_rels, $rel_name, false);

					if (!$rel)
					{
						throw new \Exception("Model '{$model->model_name}' doesn't have a '{$rel_name}' relation defined");
					}
					if ($rel['type'] == 'has_many')
					{
						throw new \Exception("Relationship '{$rel_name}' is of has_many type and cannot be preloaded view with()");
					}
					$rel_model = $this->pixie->orm->get($rel['model']);

					if ($rel['type'] == 'belongs_to')
					{
						$this->query->join(array($rel_model->table, $alias), array(
							$model_alias.'.'.$rel['key'],
							$alias.'.'.$rel_model->id_field,
							), 'left');
					}
					else
					{
						$this->query->join(array($rel_model->table, $alias), array(
							$model_alias.'.'.$model->id_field,
							$alias.'.'.$rel['key'],
							), 'left');
					}

					foreach ($rel_model->columns() as $column)
					{
						$fields[] = array("{$alias}.{$column}", "{$alias}__{$column}");
					}
					$model = $rel_model;
					$model_alias = $alias;
					$paths[$path] = array('alias' => $alias, 'model' => $model);
				}
			}

			call_user_func_array(array($this->query, 'fields'), $fields);
		}
		
		return $paths;
		
	}
	
	/**
	 * Finds all rows that meet set criteria.
	 *
	 * @return \PHPixie\ORM\Result Returns ORM Result that you can use in a 'foreach' loop.
	 */
	public function find_all()
	{
		$paths = $this->prepare_relations();
		return $this->pixie->orm->result($this->model_name, $this->query->execute(), $paths);
	}

	/**
	 * Searches for the first row that meets set criteria. If no rows match it still returns an ORM model
	 * but with its loaded() flag being False. calling save() on such an object will insert a new row.
	 *
	 * @return \PHPixie\ORM\Model Found item or new object of the current model but with loaded() flag being False
	 */
	public function find()
	{
		$set_limit = $this->limit();
		$res = $this->limit(1)->find_all()->current();
		$this->limit($set_limit);
		return $res;
	}

	/**
	 * Counts all rows that meet set criteria. Ignores limit and offset.
	 *
	 * @return int Number of rows
	 */
	public function count_all()
	{
		$query = clone $this->query;
		$query->type('count');
		return $query->execute();
	}

	/**
	 * Checks if the item is considered to be loaded from the database
	 *
	 * @return boolean Returns True if the item was loaded
	 */
	public function loaded()
	{
		return $this->_loaded;
	}

	/**
	 * Returns the row associated with current ORM item as an associative array
	 *
	 * @return array  Associative array representing the row
	 */
	public function as_array()
	{
		return $this->_row;
	}

	/**
	 * Returns a clone of  query builder that is being used to set conditions.
	 * It is useful for example if you let ORM manage building a complex query using it's relationship
	 * system, then you get the clone of that query and alter it to your liking,
	 * so there is no need to writing relationship joins yourself.
	 *
	 * @return \PHPixie\DB\Query A clone of the current query builder
	 */
	public function query()
	{
		return clone $this->query;
	}

	/**
	 * You can override this method to return additional properties that you would like to use
	 * in your model. One advantage for using this instead of just overriding __get() is that
	 * in this way the properties also get cached.
	 *
	 * @param string $property The name of the property to get
	 * @return void
	 */
	public function get($property)
	{

	}

	/**
	 * Magic method that allows to chech if the propert exists on the model.
	 * Takes relationships and properties defined using get() into account.
	 * @param string   $column Name of the column, property or relationship to get
	 * @return boolean If the property exists
	 */
	public function __isset($property) {
		if (array_key_exists($property, $this->_row))
			return true;
		if (array_key_exists($property, $this->cached))
			return true;
		if (($val = $this->get($property)) !== null)
			return true;
		$relations = array_merge($this->has_one, $this->has_many, $this->belongs_to);
		if ($target = $this->pixie->arr($relations, $property, false))
			return true;
		return false;		
	}
	
	/**
	 * Magic method that allows accessing row columns and extensions as properties and also facilitates
	 * access to relationships and custom properties defined in get() method.
	 * If a relationship is being accessed, it will return an ORM model of the related table
	 * and automatically alter its query so that all your previously set conditions will remain

	 * @param string   $column Name of the column, property or relationship to get
	 * @return mixed   Requested property
	 * @throws \Exception If neither property nor a relationship with such name is found
	 */
	public function __get($column)
	{
		if (array_key_exists($column, $this->_row))
			return $this->_row[$column];
			
		if (array_key_exists($column, $this->cached))
			return $this->cached[$column];
			
		if (($val = $this->get($column)) !== null)
		{
			$this->cached[$column] = $val;
			return $val;
		}
		
		if (array_key_exists($column, $this->extension_instances))
		{
			return $this->extension_instances[$column];
		}
		
		if (array_key_exists($column, $this->extensions))
		{	
			return $this->extension_instances[$column] = 
				$this->pixie->orm->extension($this->extensions[$column], $this);
		}
		
		$relations = array_merge($this->has_one, $this->has_many, $this->belongs_to);
		if ($target = $this->pixie->arr($relations, $column, false))
		{
			$model = $this->pixie->orm->get($target['model']);
			$model->query = clone $this->query;
			if ($this->loaded())
			{
				$model->query->where($this->id_field, $this->_row[$this->id_field]);
			}
			if ($target['type'] == 'has_many' && isset($target['through']))
			{
				$last_alias = $model->query->last_alias();
				$through_alias = $model->query->add_alias();
				$new_alias = $model->query->add_alias();
				$model->query->join(array($target['through'], $through_alias), array(
					$last_alias.'.'.$this->id_field,
					$through_alias.'.'.$target['key'],
					), 'inner');
				$model->query->join(array($model->table, $new_alias), array(
					$through_alias.'.'.$target['foreign_key'],
					$new_alias.'.'.$model->id_field,
					), 'inner');
			}
			else
			{
				$last_alias = $model->query->last_alias();
				$new_alias = $model->query->add_alias();
				if ($target['type'] == 'belongs_to')
				{
					$model->query->join(array($model->table, $new_alias), array(
						$last_alias.'.'.$target['key'],
						$new_alias.'.'.$model->id_field,
						), 'inner');
				}
				else
				{
					$model->query->join(array($model->table, $new_alias), array(
						$last_alias.'.'.$this->id_field,
						$new_alias.'.'.$target['key'],
						), 'inner');
				}
			}
			$model->query->fields("$new_alias.*");
			if ($target['type'] != 'has_many' && $this->loaded())
			{
				$model = $model->find();
				$this->cached[$column] = $model;
			}
			return $model;
		}

		throw new \Exception("Property {$column} not found on {$this->model_name} model.");
	}

	/**
	 * Magic method to update record values when set as properties or to add an ORM item to
	 * a relation. By assigning an ORM model to a relationship a relationship is created between the
	 * current item and the passed one  Using properties this way is a shortcut to the add() method.
	 *
	 * @param string $column Column or relationship name
	 * @param mixed $val    Column value or an ORM model to be added to a relation
	 * @return void
	 * @see add()
	 */
	public function __set($column, $val)
	{
		$relations = array_merge($this->has_one, $this->has_many, $this->belongs_to);
		if (array_key_exists($column, $relations))
		{
			$this->add($column, $val);
		}
		else
		{
			$this->_row[$column] = $val;
		}
		$this->cached = array();
	}

	/**
	 * Create a relationship between current item and an other one
	 *
	 * @param string   $relation Name of the relationship
	 * @param \PHPixie\ORM\Model    $model    ORM item to create a relationship with
	 * @return void
	 * @throws \Exception If relationship is not defined
	 * @throws \Exception If current item is not in the database yet (isn't considered loaded())
	 * @throws \Exception If passed item is not in the database yet (isn't considered loaded())
	 */
	public function add($relation, $model)
	{

		$rels = array_merge($this->has_one, $this->has_many, $this->belongs_to);
		$rel = $this->pixie->arr($rels, $relation, false);
		if (!$rel)
		{
			throw new \Exception("Model doesn't have a '{$relation}' relation defined");
		}

		if ($rel['type'] == 'belongs_to')
		{

			if (!$model->loaded())
			{
				throw new \Exception("Model must be loaded before added to a belongs_to relationship. Probably you haven't saved it.");
			}

			$key = $rel['key'];
			$this->$key = $model->_row[$this->id_field];
			if ($this->loaded())
			{
				$this->save();
			}
		}
		elseif (isset($rel['through']))
		{

			if (!$this->loaded())
			{
				throw new \Exception("Model must be loaded before you try adding 'through' relationships to it. Probably you haven't saved it.");
			}
			if (!$model->loaded())
			{
				throw new \Exception("Model must be loaded before added to a 'through' relationship. Probably you haven't saved it.");
			}

			$exists = $this->conn->query('count')
				->table($rel['through'])
				->where(array(
					array($rel['key'], $this->_row[$this->id_field]),
					array($rel['foreign_key'], $model->_row[$model->id_field])
				))
				->execute();
			if (!$exists)
			{
				$this->conn->query('insert')
					->table($rel['through'])
					->data(array(
						$rel['key'] => $this->_row[$this->id_field],
						$rel['foreign_key'] => $model->_row[$model->id_field]
					))
					->execute();
			}
		}
		else
		{

			if (!$this->loaded())
			{
				throw new \Exception("Model must be loaded before you try adding 'has_many' relationships to it. Probably you haven't saved it.");
			}

			$key = $rel['key'];
			$model->$key = $this->_row[$this->id_field];
			if ($model->loaded())
			{
				$model->save();
			}
		}
		$this->cached = array();
	}

	/**
	 * Removes a relationship between current item and the passed one
	 *
	 * @param string   $relation Name of the relationship
	 * @param \PHPixie\ORM\Model    $model    ORM item to remove relationship with. Can be omitted for 'belongs_to' relationships
	 * @return void
	 * @throws \Exception If realtionship is not defined
	 * @throws \Exception If current item is not in the database yet (isn't considered loaded())
	 * @throws \Exception If passed item is not in the database yet (isn't considered loaded())
	 */
	public function remove($relation, $model = null)
	{

		if (!$this->loaded())
		{
			throw new \Exception("Model must be loaded before you try removing relationships from it.");
		}

		$rels = array_merge($this->has_one, $this->has_many, $this->belongs_to);
		$rel = $this->pixie->arr($rels, $relation, false);
		if (!$rel)
		{
			throw new \Exception("Model doesn't have a '{$relation}' relation defined");
		}

		if ($rel['type'] != 'belongs_to' && (!$model || !$model->loaded()))
		{
			throw new \Exception("Model must be loaded before being removed from a has_one or has_many relationship.");
		}
		if ($rel['type'] == 'belongs_to')
		{
			$key = $rel['key'];
			$this->$key = null;
			$this->save();
		}
		elseif (isset($rel['through']))
		{
			$this->conn->query('delete')
				->table($rel['through'])
				->where(array(
					array($rel['key'], $this->_row[$this->id_field]),
					array($rel['foreign_key'], $model->_row[$model->id_field])
				))
				->execute();
		}
		else
		{
			$key = $rel['key'];
			$model->$key = null;
			$model->save();
		}
		$this->cached = array();
	}

	/**
	 * Gets name column names of the table associated with the model.
	 *
	 * @return array   Array of column names
	 */
	public function columns()
	{
		$cache = &$this->pixie->orm->column_cache;
		
		if (!isset($cache[$this->table]))
			$cache[$this->table] = $this->conn->list_columns($this->table);
		return $cache[$this->table];
	}

	/**
	 * Gets the items id field value
	 *
	 * @return mixed   Item id
	 */
	public function id() 
	{
		if ($this->loaded())
			return $this->_row[$this->id_field];
			
		return null;
	}
	
	/**
	 * Defines which relationships should be preloaded. You can only preload
	 * belongs_to and has_one relationships. You can use the dot notation to
	 * preload deep relationsips, e.g. 'tree.protector' will preload the tree
	 * that a fairy lives in and also preload the protector of that tree.
	 *
	 * @param string $relationsip,...   List of relationships to preload
	 * @return \PHPixie\ORM\Model Returns itself
	 */
	public function with()
	{
		$this->_with = func_get_args();
		return $this;
	}

	/**
	 * Deletes current item from the database
	 *
	 * @return void
	 * @throws \Exception If the item is not in the database, e.g. is not loaded()
	 */
	public function delete()
	{
		if (!$this->loaded())
		{
			throw new \Exception("Cannot delete an item that wasn't selected from database");
		}
		$this->conn->query('delete')
			->table($this->table)
			->where($this->id_field, $this->_row[$this->id_field])
			->execute();
		$this->cached = array();
	}

	/**
	 * Deletes all items that meet set conditions. Use in the same way
	 * as you would a find_all() method.
	 *
	 * @return \PHPixie\ORM\Model Returns self
	 */
	public function delete_all()
	{
		$query = clone $this->query;
		$query->type('delete');
		$query->execute();
		return $this;
	}

	/**
	 * Saves the item back to the database. If item is loaded() it will result
	 * in an update, otherwise a new row will be inserted
	 *
	 * @return \PHPixie\ORM\Model Returns self
	 */
	public function save()
	{
		if ($this->loaded())
		{
			$query = $this->conn->query('update')
				->table($this->table)
				->where($this->id_field, $this->_row[$this->id_field]);
		}
		else
		{
			$query = $this->conn->query('insert')
				->table($this->table);
		}
		$query->data($this->_row);
		$query->execute();

		if ($this->loaded())
		{
			$id = $this->_row[$this->id_field];
		}
		else
		{
			$id = $this->conn->insert_id();
		}
		$row = (array) $this->conn->query('select')
				->table($this->table)
				->where($this->id_field, $id)->execute()->current();
		$this->values($row, true);
		return $this;
	}

	/**
	 * Batch updates item columns using an associative array
	 *
	 * @param array $row        Associative array of key => value pairs
	 * @param boolean $set_loaded Flag to consider the ORM item loaded. Useful if you selected
	 *                            the row from the database and want to wrap it in ORM
	 * @return \PHPixie\ORM\Model Returns self
	 */
	public function values($row, $set_loaded = false)
	{
		$this->_row = array_merge($this->_row, $row);
		if ($set_loaded)
		{
			$this->_loaded = true;
		}
		$this->cached = array();
		return $this;
	}

	/**
	 * Gets plural form of a noun
	 *
	 * @param string  $str Noun to get a plural form of
	 * @return string  Plural form
	 */
	protected function plural($str)
	{
		$regexes = array(
			'/^(.*?[sxz])$/i' => '\\1es',
			'/^(.*?[^aeioudgkprt]h)$/i' => '\\1es',
			'/^(.*?[^aeiou])y$/i' => '\\1ies',
		);
		foreach ($regexes as $key => $val)
		{
			$str = preg_replace($key, $val, $str, -1, $count);
			if ($count)
			{
				return $str;
			}
		}
		return $str.'s';
	}

	/**
	 * Gets singular form of a noun
	 *
	 * @param string $str Noun to get singular form of
	 * @return string Singular form of the noun
	 */
	protected function singular($str)
	{
		$regexes = array(
			'/^(.*?us)$/i' => '\\1',
			'/^(.*?[sxz])es$/i' => '\\1',
			'/^(.*?[^aeioudgkprt]h)es$/i' => '\\1',
			'/^(.*?[^aeiou])ies$/i' => '\\1y',
			'/^(.*?)s$/' => '\\1',
		);
		foreach ($regexes as $key => $val)
		{
			$str = preg_replace($key, $val, $str, -1, $count);
			if ($count)
			{
				return $str;
			}
		}
		return $str;
	}

}
