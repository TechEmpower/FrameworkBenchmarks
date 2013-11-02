<?php
/**
 * @package ActiveRecord
 */
namespace ActiveRecord;

/**
 * The base class for your models.
 *
 * Defining an ActiveRecord model for a table called people and orders:
 *
 * <code>
 * CREATE TABLE people(
 *   id int primary key auto_increment,
 *   parent_id int,
 *   first_name varchar(50),
 *   last_name varchar(50)
 * );
 *
 * CREATE TABLE orders(
 *   id int primary key auto_increment,
 *   person_id int not null,
 *   cost decimal(10,2),
 *   total decimal(10,2)
 * );
 * </code>
 *
 * <code>
 * class Person extends ActiveRecord\Model {
 *   static $belongs_to = array(
 *     array('parent', 'foreign_key' => 'parent_id', 'class_name' => 'Person')
 *   );
 *
 *   static $has_many = array(
 *     array('children', 'foreign_key' => 'parent_id', 'class_name' => 'Person'),
 *     array('orders')
 *   );
 *
 *   static $validates_length_of = array(
 *     array('first_name', 'within' => array(1,50)),
 *     array('last_name', 'within' => array(1,50))
 *   );
 * }
 *
 * class Order extends ActiveRecord\Model {
 *   static $belongs_to = array(
 *     array('person')
 *   );
 *
 *   static $validates_numericality_of = array(
 *     array('cost', 'greater_than' => 0),
 *     array('total', 'greater_than' => 0)
 *   );
 *
 *   static $before_save = array('calculate_total_with_tax');
 *
 *   public function calculate_total_with_tax() {
 *     $this->total = $this->cost * 0.045;
 *   }
 * }
 * </code>
 *
 * For a more in-depth look at defining models, relationships, callbacks and many other things
 * please consult our {@link http://www.phpactiverecord.org/guides Guides}.
 *
 * @package ActiveRecord
 * @see BelongsTo
 * @see CallBack
 * @see HasMany
 * @see HasAndBelongsToMany
 * @see Serialization
 * @see Validations
 */
class Model
{
	/**
	 * An instance of {@link Errors} and will be instantiated once a write method is called.
	 *
	 * @var Errors
	 */
	public $errors;

	/**
	 * Contains model values as column_name => value
	 *
	 * @var array
	 */
	private $attributes = array();

	/**
	 * Flag whether or not this model's attributes have been modified since it will either be null or an array of column_names that have been modified
	 *
	 * @var array
	 */
	private $__dirty = null;

	/**
	 * Flag that determines of this model can have a writer method invoked such as: save/update/insert/delete
	 *
	 * @var boolean
	 */
	private $__readonly = false;

	/**
	 * Array of relationship objects as model_attribute_name => relationship
	 *
	 * @var array
	 */
	private $__relationships = array();

	/**
	 * Flag that determines if a call to save() should issue an insert or an update sql statement
	 *
	 * @var boolean
	 */
	private $__new_record = true;

	/**
	 * Set to the name of the connection this {@link Model} should use.
	 *
	 * @var string
	 */
	static $connection;

	/**
	 * Set to the name of the database this Model's table is in.
	 *
	 * @var string
	 */
	static $db;

	/**
	 * Set this to explicitly specify the model's table name if different from inferred name.
	 *
	 * If your table doesn't follow our table name convention you can set this to the
	 * name of your table to explicitly tell ActiveRecord what your table is called.
	 *
	 * @var string
	 */
	static $table_name;

	/**
	 * Set this to override the default primary key name if different from default name of "id".
	 *
	 * @var string
	 */
	static $primary_key;

	/**
	 * Set this to explicitly specify the sequence name for the table.
	 *
	 * @var string
	 */
	static $sequence;

	/**
	 * Allows you to create aliases for attributes.
	 *
	 * <code>
	 * class Person extends ActiveRecord\Model {
	 *   static $alias_attribute = array(
	 *     'alias_first_name' => 'first_name',
	 *     'alias_last_name' => 'last_name');
	 * }
	 *
	 * $person = Person::first();
	 * $person->alias_first_name = 'Tito';
	 * echo $person->alias_first_name;
	 * </code>
	 *
	 * @var array
	 */
	static $alias_attribute = array();

	/**
	 * Whitelist of attributes that are checked from mass-assignment calls such as constructing a model or using update_attributes.
	 *
	 * This is the opposite of {@link attr_protected $attr_protected}.
	 *
	 * <code>
	 * class Person extends ActiveRecord\Model {
	 *   static $attr_accessible = array('first_name','last_name');
	 * }
	 *
	 * $person = new Person(array(
	 *   'first_name' => 'Tito',
	 *   'last_name' => 'the Grief',
	 *   'id' => 11111));
	 *
	 * echo $person->id; # => null
	 * </code>
	 *
	 * @var array
	 */
	static $attr_accessible = array();

	/**
	 * Blacklist of attributes that cannot be mass-assigned.
	 *
	 * This is the opposite of {@link attr_accessible $attr_accessible} and the format
	 * for defining these are exactly the same.
	 *
	 * If the attribute is both accessible and protected, it is treated as protected.
	 *
	 * @var array
	 */
	static $attr_protected = array();

	/**
	 * Delegates calls to a relationship.
	 *
	 * <code>
	 * class Person extends ActiveRecord\Model {
	 *   static $belongs_to = array(array('venue'),array('host'));
	 *   static $delegate = array(
	 *     array('name', 'state', 'to' => 'venue'),
	 *     array('name', 'to' => 'host', 'prefix' => 'woot'));
	 * }
	 * </code>
	 *
	 * Can then do:
	 *
	 * <code>
	 * $person->state     # same as calling $person->venue->state
	 * $person->name      # same as calling $person->venue->name
	 * $person->woot_name # same as calling $person->host->name
	 * </code>
	 *
	 * @var array
	 */
	static $delegate = array();

	/**
	 * Constructs a model.
	 *
	 * When a user instantiates a new object (e.g.: it was not ActiveRecord that instantiated via a find)
	 * then @var $attributes will be mapped according to the schema's defaults. Otherwise, the given
	 * $attributes will be mapped via set_attributes_via_mass_assignment.
	 *
	 * <code>
	 * new Person(array('first_name' => 'Tito', 'last_name' => 'the Grief'));
	 * </code>
	 *
	 * @param array $attributes Hash containing names and values to mass assign to the model
	 * @param boolean $guard_attributes Set to true to guard protected/non-accessible attributes
	 * @param boolean $instantiating_via_find Set to true if this model is being created from a find call
	 * @param boolean $new_record Set to true if this should be considered a new record
	 * @return Model
	 */
	public function __construct(array $attributes=array(), $guard_attributes=true, $instantiating_via_find=false, $new_record=true)
	{
		$this->__new_record = $new_record;

		// initialize attributes applying defaults
		if (!$instantiating_via_find)
		{
			foreach (static::table()->columns as $name => $meta)
				$this->attributes[$meta->inflected_name] = $meta->default;
		}

		$this->set_attributes_via_mass_assignment($attributes, $guard_attributes);

		// since all attribute assignment now goes thru assign_attributes() we want to reset
		// dirty if instantiating via find since nothing is really dirty when doing that
		if ($instantiating_via_find)
			$this->__dirty = array();

		$this->invoke_callback('after_construct',false);
	}

	/**
	 * Magic method which delegates to read_attribute(). This handles firing off getter methods,
	 * as they are not checked/invoked inside of read_attribute(). This circumvents the problem with
	 * a getter being accessed with the same name as an actual attribute.
	 *
	 * You can also define customer getter methods for the model.
	 *
	 * EXAMPLE:
	 * <code>
	 * class User extends ActiveRecord\Model {
	 *
	 *   # define custom getter methods. Note you must
	 *   # prepend get_ to your method name:
	 *   function get_middle_initial() {
	 *     return $this->middle_name{0};
	 *   }
	 * }
	 *
	 * $user = new User();
	 * echo $user->middle_name;  # will call $user->get_middle_name()
	 * </code>
	 *
	 * If you define a custom getter with the same name as an attribute then you
	 * will need to use read_attribute() to get the attribute's value.
	 * This is necessary due to the way __get() works.
	 *
	 * For example, assume 'name' is a field on the table and we're defining a
	 * custom getter for 'name':
	 *
	 * <code>
	 * class User extends ActiveRecord\Model {
	 *
	 *   # INCORRECT way to do it
	 *   # function get_name() {
	 *   #   return strtoupper($this->name);
	 *   # }
	 *
	 *   function get_name() {
	 *     return strtoupper($this->read_attribute('name'));
	 *   }
	 * }
	 *
	 * $user = new User();
	 * $user->name = 'bob';
	 * echo $user->name; # => BOB
	 * </code>
	 *
	 *
	 * @see read_attribute()
	 * @param string $name Name of an attribute
	 * @return mixed The value of the attribute
	 */
	public function &__get($name)
	{
		// check for getter
		if (method_exists($this, "get_$name"))
		{
			$name = "get_$name";
			$value = $this->$name();
			return $value;
		}

		return $this->read_attribute($name);
	}

	/**
	 * Determines if an attribute exists for this {@link Model}.
	 *
	 * @param string $attribute_name
	 * @return boolean
	 */
	public function __isset($attribute_name)
	{
		return array_key_exists($attribute_name,$this->attributes) || array_key_exists($attribute_name,static::$alias_attribute);
	}

	/**
	 * Magic allows un-defined attributes to set via $attributes.
	 *
	 * You can also define customer setter methods for the model.
	 *
	 * EXAMPLE:
	 * <code>
	 * class User extends ActiveRecord\Model {
	 *
	 *   # define custom setter methods. Note you must
	 *   # prepend set_ to your method name:
	 *   function set_password($plaintext) {
	 *     $this->encrypted_password = md5($plaintext);
	 *   }
	 * }
	 *
	 * $user = new User();
	 * $user->password = 'plaintext';  # will call $user->set_password('plaintext')
	 * </code>
	 *
	 * If you define a custom setter with the same name as an attribute then you
	 * will need to use assign_attribute() to assign the value to the attribute.
	 * This is necessary due to the way __set() works.
	 *
	 * For example, assume 'name' is a field on the table and we're defining a
	 * custom setter for 'name':
	 *
	 * <code>
	 * class User extends ActiveRecord\Model {
	 *
	 *   # INCORRECT way to do it
	 *   # function set_name($name) {
	 *   #   $this->name = strtoupper($name);
	 *   # }
	 *
	 *   function set_name($name) {
	 *     $this->assign_attribute('name',strtoupper($name));
	 *   }
	 * }
	 *
	 * $user = new User();
	 * $user->name = 'bob';
	 * echo $user->name; # => BOB
	 * </code>
	 *
	 * @throws {@link UndefinedPropertyException} if $name does not exist
	 * @param string $name Name of attribute, relationship or other to set
	 * @param mixed $value The value
	 * @return mixed The value
	 */
	public function __set($name, $value)
	{
		if (array_key_exists($name, static::$alias_attribute))
			$name = static::$alias_attribute[$name];

		elseif (method_exists($this,"set_$name"))
		{
			$name = "set_$name";
			return $this->$name($value);
		}

		if (array_key_exists($name,$this->attributes))
			return $this->assign_attribute($name,$value);

		if ($name == 'id')
			return $this->assign_attribute($this->get_primary_key(true),$value);

		foreach (static::$delegate as &$item)
		{
			if (($delegated_name = $this->is_delegated($name,$item)))
				return $this->$item['to']->$delegated_name = $value;
		}

		throw new UndefinedPropertyException(get_called_class(),$name);
	}

	public function __wakeup()
	{
		// make sure the models Table instance gets initialized when waking up
		static::table();
	}

	/**
	 * Assign a value to an attribute.
	 *
	 * @param string $name Name of the attribute
	 * @param mixed &$value Value of the attribute
	 * @return mixed the attribute value
	 */
	public function assign_attribute($name, $value)
	{
		$table = static::table();
		if (!is_object($value)) {
			if (array_key_exists($name, $table->columns)) {
				$value = $table->columns[$name]->cast($value, static::connection());
			} else {
				$col = $table->get_column_by_inflected_name($name);
				if (!is_null($col)){
					$value = $col->cast($value, static::connection());
				}
			}
		}

		// convert php's \DateTime to ours
		if ($value instanceof \DateTime)
			$value = new DateTime($value->format('Y-m-d H:i:s T'));

		// make sure DateTime values know what model they belong to so
		// dirty stuff works when calling set methods on the DateTime object
		if ($value instanceof DateTime)
			$value->attribute_of($this,$name);

		$this->attributes[$name] = $value;
		$this->flag_dirty($name);
		return $value;
	}

	/**
	 * Retrieves an attribute's value or a relationship object based on the name passed. If the attribute
	 * accessed is 'id' then it will return the model's primary key no matter what the actual attribute name is
	 * for the primary key.
	 *
	 * @param string $name Name of an attribute
	 * @return mixed The value of the attribute
	 * @throws {@link UndefinedPropertyException} if name could not be resolved to an attribute, relationship, ...
	 */
	public function &read_attribute($name)
	{
		// check for aliased attribute
		if (array_key_exists($name, static::$alias_attribute))
			$name = static::$alias_attribute[$name];

		// check for attribute
		if (array_key_exists($name,$this->attributes))
			return $this->attributes[$name];

		// check relationships if no attribute
		if (array_key_exists($name,$this->__relationships))
			return $this->__relationships[$name];

		$table = static::table();

		// this may be first access to the relationship so check Table
		if (($relationship = $table->get_relationship($name)))
		{
			$this->__relationships[$name] = $relationship->load($this);
			return $this->__relationships[$name];
		}

		if ($name == 'id')
		{
			$pk = $this->get_primary_key(true);
			if (isset($this->attributes[$pk]))
				return $this->attributes[$pk];
		}

		//do not remove - have to return null by reference in strict mode
		$null = null;

		foreach (static::$delegate as &$item)
		{
			if (($delegated_name = $this->is_delegated($name,$item)))
			{
				$to = $item['to'];
				if ($this->$to)
				{
					$val =& $this->$to->__get($delegated_name);
					return $val;
				}
				else
					return $null;
			}
		}

		throw new UndefinedPropertyException(get_called_class(),$name);
	}

	/**
	 * Flags an attribute as dirty.
	 *
	 * @param string $name Attribute name
	 */
	public function flag_dirty($name)
	{
		if (!$this->__dirty)
			$this->__dirty = array();

		$this->__dirty[$name] = true;
	}

	/**
	 * Returns hash of attributes that have been modified since loading the model.
	 *
	 * @return mixed null if no dirty attributes otherwise returns array of dirty attributes.
	 */
	public function dirty_attributes()
	{
		if (!$this->__dirty)
			return null;

		$dirty = array_intersect_key($this->attributes,$this->__dirty);
		return !empty($dirty) ? $dirty : null;
	}

	/**
	 * Check if a particular attribute has been modified since loading the model.
	 * @param string $attribute	Name of the attribute
	 * @return boolean TRUE if it has been modified.
	 */
	public function attribute_is_dirty($attribute)
	{
		return $this->__dirty && $this->__dirty[$attribute] && array_key_exists($attribute, $this->attributes);
	}

	/**
	 * Returns a copy of the model's attributes hash.
	 *
	 * @return array A copy of the model's attribute data
	 */
	public function attributes()
	{
		return $this->attributes;
	}

	/**
	 * Retrieve the primary key name.
	 *
	 * @param boolean Set to true to return the first value in the pk array only
	 * @return string The primary key for the model
	 */
	public function get_primary_key($first=false)
	{
		$pk = static::table()->pk;
		return $first ? $pk[0] : $pk;
	}

	/**
	 * Returns the actual attribute name if $name is aliased.
	 *
	 * @param string $name An attribute name
	 * @return string
	 */
	public function get_real_attribute_name($name)
	{
		if (array_key_exists($name,$this->attributes))
			return $name;

		if (array_key_exists($name,static::$alias_attribute))
			return static::$alias_attribute[$name];

		return null;
	}

	/**
	 * Returns array of validator data for this Model.
	 *
	 * Will return an array looking like:
	 *
	 * <code>
	 * array(
	 *   'name' => array(
	 *     array('validator' => 'validates_presence_of'),
	 *     array('validator' => 'validates_inclusion_of', 'in' => array('Bob','Joe','John')),
	 *   'password' => array(
	 *     array('validator' => 'validates_length_of', 'minimum' => 6))
	 *   )
	 * );
	 * </code>
	 *
	 * @return array An array containing validator data for this model.
	 */
	public function get_validation_rules()
	{
		require_once 'Validations.php';

		$validator = new Validations($this);
		return $validator->rules();
	}

	/**
	 * Returns an associative array containing values for all the attributes in $attributes
	 *
	 * @param array $attributes Array containing attribute names
	 * @return array A hash containing $name => $value
	 */
	public function get_values_for($attributes)
	{
		$ret = array();

		foreach ($attributes as $name)
		{
			if (array_key_exists($name,$this->attributes))
				$ret[$name] = $this->attributes[$name];
		}
		return $ret;
	}

	/**
	 * Retrieves the name of the table for this Model.
	 *
	 * @return string
	 */
	public static function table_name()
	{
		return static::table()->table;
	}

	/**
	 * Returns the attribute name on the delegated relationship if $name is
	 * delegated or null if not delegated.
	 *
	 * @param string $name Name of an attribute
	 * @param array $delegate An array containing delegate data
	 * @return delegated attribute name or null
	 */
	private function is_delegated($name, &$delegate)
	{
		if ($delegate['prefix'] != '')
			$name = substr($name,strlen($delegate['prefix'])+1);

		if (is_array($delegate) && in_array($name,$delegate['delegate']))
			return $name;

		return null;
	}

	/**
	 * Determine if the model is in read-only mode.
	 *
	 * @return boolean
	 */
	public function is_readonly()
	{
		return $this->__readonly;
	}

	/**
	 * Determine if the model is a new record.
	 *
	 * @return boolean
	 */
	public function is_new_record()
	{
		return $this->__new_record;
	}

	/**
	 * Throws an exception if this model is set to readonly.
	 *
	 * @throws ActiveRecord\ReadOnlyException
	 * @param string $method_name Name of method that was invoked on model for exception message
	 */
	private function verify_not_readonly($method_name)
	{
		if ($this->is_readonly())
			throw new ReadOnlyException(get_class($this), $method_name);
	}

	/**
	 * Flag model as readonly.
	 *
	 * @param boolean $readonly Set to true to put the model into readonly mode
	 */
	public function readonly($readonly=true)
	{
		$this->__readonly = $readonly;
	}

	/**
	 * Retrieve the connection for this model.
	 *
	 * @return Connection
	 */
	public static function connection()
	{
		return static::table()->conn;
	}

	/**
	 * Re-establishes the database connection with a new connection.
	 *
	 * @return Connection
	 */
	public static function reestablish_connection()
	{
		return static::table()->reestablish_connection();
	}

	/**
	 * Returns the {@link Table} object for this model.
	 *
	 * Be sure to call in static scoping: static::table()
	 *
	 * @return Table
	 */
	public static function table()
	{
		return Table::load(get_called_class());
	}

	/**
	 * Creates a model and saves it to the database.
	 *
	 * @param array $attributes Array of the models attributes
	 * @param boolean $validate True if the validators should be run
	 * @return Model
	 */
	public static function create($attributes, $validate=true)
	{
		$class_name = get_called_class();
		$model = new $class_name($attributes);
		$model->save($validate);
		return $model;
	}

	/**
	 * Save the model to the database.
	 *
	 * This function will automatically determine if an INSERT or UPDATE needs to occur.
	 * If a validation or a callback for this model returns false, then the model will
	 * not be saved and this will return false.
	 *
	 * If saving an existing model only data that has changed will be saved.
	 *
	 * @param boolean $validate Set to true or false depending on if you want the validators to run or not
	 * @return boolean True if the model was saved to the database otherwise false
	 */
	public function save($validate=true)
	{
		$this->verify_not_readonly('save');
		return $this->is_new_record() ? $this->insert($validate) : $this->update($validate);
	}

	/**
	 * Issue an INSERT sql statement for this model's attribute.
	 *
	 * @see save
	 * @param boolean $validate Set to true or false depending on if you want the validators to run or not
	 * @return boolean True if the model was saved to the database otherwise false
	 */
	private function insert($validate=true)
	{
		$this->verify_not_readonly('insert');

		if (($validate && !$this->_validate() || !$this->invoke_callback('before_create',false)))
			return false;

		$table = static::table();

		if (!($attributes = $this->dirty_attributes()))
			$attributes = $this->attributes;

		$pk = $this->get_primary_key(true);
		$use_sequence = false;

		if ($table->sequence && !isset($attributes[$pk]))
		{
			if (($conn = static::connection()) instanceof OciAdapter)
			{
				// terrible oracle makes us select the nextval first
				$attributes[$pk] = $conn->get_next_sequence_value($table->sequence);
				$table->insert($attributes);
				$this->attributes[$pk] = $attributes[$pk];
			}
			else
			{
				// unset pk that was set to null
				if (array_key_exists($pk,$attributes))
					unset($attributes[$pk]);

				$table->insert($attributes,$pk,$table->sequence);
				$use_sequence = true;
			}
		}
		else
			$table->insert($attributes);

		// if we've got an autoincrementing/sequenced pk set it
		// don't need this check until the day comes that we decide to support composite pks
		// if (count($pk) == 1)
		{
			$column = $table->get_column_by_inflected_name($pk);

			if ($column->auto_increment || $use_sequence)
				$this->attributes[$pk] = static::connection()->insert_id($table->sequence);
		}

		$this->invoke_callback('after_create',false);
		$this->__new_record = false;
		return true;
	}

	/**
	 * Issue an UPDATE sql statement for this model's dirty attributes.
	 *
	 * @see save
	 * @param boolean $validate Set to true or false depending on if you want the validators to run or not
	 * @return boolean True if the model was saved to the database otherwise false
	 */
	private function update($validate=true)
	{
		$this->verify_not_readonly('update');

		if ($validate && !$this->_validate())
			return false;

		if ($this->is_dirty())
		{
			$pk = $this->values_for_pk();

			if (empty($pk))
				throw new ActiveRecordException("Cannot update, no primary key defined for: " . get_called_class());

			if (!$this->invoke_callback('before_update',false))
				return false;

			$dirty = $this->dirty_attributes();
			static::table()->update($dirty,$pk);
			$this->invoke_callback('after_update',false);
		}

		return true;
	}

	/**
	 * Deletes records matching conditions in $options
	 *
	 * Does not instantiate models and therefore does not invoke callbacks
	 *
	 * Delete all using a hash:
	 *
	 * <code>
	 * YourModel::delete_all(array('conditions' => array('name' => 'Tito')));
	 * </code>
	 *
	 * Delete all using an array:
	 *
	 * <code>
	 * YourModel::delete_all(array('conditions' => array('name = ?', 'Tito')));
	 * </code>
	 *
	 * Delete all using a string:
	 *
	 * <code>
	 * YourModel::delete_all(array('conditions' => 'name = "Tito"));
	 * </code>
	 *
	 * An options array takes the following parameters:
	 *
	 * <ul>
	 * <li><b>conditions:</b> Conditions using a string/hash/array</li>
	 * <li><b>limit:</b> Limit number of records to delete (MySQL & Sqlite only)</li>
	 * <li><b>order:</b> A SQL fragment for ordering such as: 'name asc', 'id desc, name asc' (MySQL & Sqlite only)</li>
	 * </ul>
	 *
	 * @params array $options
	 * return integer Number of rows affected
	 */
	public static function delete_all($options=array())
	{
		$table = static::table();
		$conn = static::connection();
		$sql = new SQLBuilder($conn, $table->get_fully_qualified_table_name());

		$conditions = is_array($options) ? $options['conditions'] : $options;

		if (is_array($conditions) && !is_hash($conditions))
			call_user_func_array(array($sql, 'delete'), $conditions);
		else
			$sql->delete($conditions);

		if (isset($options['limit']))
			$sql->limit($options['limit']);

		if (isset($options['order']))
			$sql->order($options['order']);

		$values = $sql->bind_values();
		$ret = $conn->query(($table->last_sql = $sql->to_s()), $values);
		return $ret->rowCount();
	}

	/**
	 * Updates records using set in $options
	 *
	 * Does not instantiate models and therefore does not invoke callbacks
	 *
	 * Update all using a hash:
	 *
	 * <code>
	 * YourModel::update_all(array('set' => array('name' => "Bob")));
	 * </code>
	 *
	 * Update all using a string:
	 *
	 * <code>
	 * YourModel::update_all(array('set' => 'name = "Bob"'));
	 * </code>
	 *
	 * An options array takes the following parameters:
	 *
	 * <ul>
	 * <li><b>set:</b> String/hash of field names and their values to be updated with
	 * <li><b>conditions:</b> Conditions using a string/hash/array</li>
	 * <li><b>limit:</b> Limit number of records to update (MySQL & Sqlite only)</li>
	 * <li><b>order:</b> A SQL fragment for ordering such as: 'name asc', 'id desc, name asc' (MySQL & Sqlite only)</li>
	 * </ul>
	 *
	 * @params array $options
	 * return integer Number of rows affected
	 */
	public static function update_all($options=array())
	{
		$table = static::table();
		$conn = static::connection();
		$sql = new SQLBuilder($conn, $table->get_fully_qualified_table_name());

		$sql->update($options['set']);

		if (isset($options['conditions']) && ($conditions = $options['conditions']))
		{
			if (is_array($conditions) && !is_hash($conditions))
				call_user_func_array(array($sql, 'where'), $conditions);
			else
				$sql->where($conditions);
		}

		if (isset($options['limit']))
			$sql->limit($options['limit']);

		if (isset($options['order']))
			$sql->order($options['order']);

		$values = $sql->bind_values();
		$ret = $conn->query(($table->last_sql = $sql->to_s()), $values);
		return $ret->rowCount();

	}

	/**
	 * Deletes this model from the database and returns true if successful.
	 *
	 * @return boolean
	 */
	public function delete()
	{
		$this->verify_not_readonly('delete');

		$pk = $this->values_for_pk();

		if (empty($pk))
			throw new ActiveRecordException("Cannot delete, no primary key defined for: " . get_called_class());

		if (!$this->invoke_callback('before_destroy',false))
			return false;

		static::table()->delete($pk);
		$this->invoke_callback('after_destroy',false);

		return true;
	}

	/**
	 * Helper that creates an array of values for the primary key(s).
	 *
	 * @return array An array in the form array(key_name => value, ...)
	 */
	public function values_for_pk()
	{
		return $this->values_for(static::table()->pk);
	}

	/**
	 * Helper to return a hash of values for the specified attributes.
	 *
	 * @param array $attribute_names Array of attribute names
	 * @return array An array in the form array(name => value, ...)
	 */
	public function values_for($attribute_names)
	{
		$filter = array();

		foreach ($attribute_names as $name)
			$filter[$name] = $this->$name;

		return $filter;
	}

	/**
	 * Validates the model.
	 *
	 * @return boolean True if passed validators otherwise false
	 */
	private function _validate()
	{
		require_once 'Validations.php';

		$validator = new Validations($this);
		$validation_on = 'validation_on_' . ($this->is_new_record() ? 'create' : 'update');

		foreach (array('before_validation', "before_$validation_on") as $callback)
		{
			if (!$this->invoke_callback($callback,false))
				return false;
		}

		// need to store reference b4 validating so that custom validators have access to add errors
		$this->errors = $validator->get_record();
		$validator->validate();

		foreach (array('after_validation', "after_$validation_on") as $callback)
			$this->invoke_callback($callback,false);

		if (!$this->errors->is_empty())
			return false;

		return true;
	}

	/**
	 * Returns true if the model has been modified.
	 *
	 * @return boolean true if modified
	 */
	public function is_dirty()
	{
		return empty($this->__dirty) ? false : true;
	}

	/**
	 * Run validations on model and returns whether or not model passed validation.
	 *
	 * @see is_invalid
	 * @return boolean
	 */
	public function is_valid()
	{
		return $this->_validate();
	}

	/**
	 * Runs validations and returns true if invalid.
	 *
	 * @see is_valid
	 * @return boolean
	 */
	public function is_invalid()
	{
		return !$this->_validate();
	}

	/**
	 * Updates a model's timestamps.
	 */
	public function set_timestamps()
	{
		$now = date('Y-m-d H:i:s');

		if (isset($this->updated_at))
			$this->updated_at = $now;

		if (isset($this->created_at) && $this->is_new_record())
			$this->created_at = $now;
	}

	/**
	 * Mass update the model with an array of attribute data and saves to the database.
	 *
	 * @param array $attributes An attribute data array in the form array(name => value, ...)
	 * @return boolean True if successfully updated and saved otherwise false
	 */
	public function update_attributes($attributes)
	{
		$this->set_attributes($attributes);
		return $this->save();
	}

	/**
	 * Updates a single attribute and saves the record without going through the normal validation procedure.
	 *
	 * @param string $name Name of attribute
	 * @param mixed $value Value of the attribute
	 * @return boolean True if successful otherwise false
	 */
	public function update_attribute($name, $value)
	{
		$this->__set($name, $value);
		return $this->update(false);
	}

	/**
	 * Mass update the model with data from an attributes hash.
	 *
	 * Unlike update_attributes() this method only updates the model's data
	 * but DOES NOT save it to the database.
	 *
	 * @see update_attributes
	 * @param array $attributes An array containing data to update in the form array(name => value, ...)
	 */
	public function set_attributes(array $attributes)
	{
		$this->set_attributes_via_mass_assignment($attributes, true);
	}

	/**
	 * Passing $guard_attributes as true will throw an exception if an attribute does not exist.
	 *
	 * @throws ActiveRecord\UndefinedPropertyException
	 * @param array $attributes An array in the form array(name => value, ...)
	 * @param boolean $guard_attributes Flag of whether or not protected/non-accessible attributes should be guarded
	 */
	private function set_attributes_via_mass_assignment(array &$attributes, $guard_attributes)
	{
		//access uninflected columns since that is what we would have in result set
		$table = static::table();
		$exceptions = array();
		$use_attr_accessible = !empty(static::$attr_accessible);
		$use_attr_protected = !empty(static::$attr_protected);
		$connection = static::connection();

		foreach ($attributes as $name => $value)
		{
			// is a normal field on the table
			if (array_key_exists($name,$table->columns))
			{
				$value = $table->columns[$name]->cast($value,$connection);
				$name = $table->columns[$name]->inflected_name;
			}

			if ($guard_attributes)
			{
				if ($use_attr_accessible && !in_array($name,static::$attr_accessible))
					continue;

				if ($use_attr_protected && in_array($name,static::$attr_protected))
					continue;

				// set valid table data
				try {
					$this->$name = $value;
				} catch (UndefinedPropertyException $e) {
					$exceptions[] = $e->getMessage();
				}
			}
			else
			{
				// ignore OciAdapter's limit() stuff
				if ($name == 'ar_rnum__')
					continue;

				// set arbitrary data
				$this->assign_attribute($name,$value);
			}
		}

		if (!empty($exceptions))
			throw new UndefinedPropertyException(get_called_class(),$exceptions);
	}

	/**
	 * Add a model to the given named ($name) relationship.
	 *
	 * @internal This should <strong>only</strong> be used by eager load
	 * @param Model $model
	 * @param $name of relationship for this table
	 * @return void
	 */
	public function set_relationship_from_eager_load(Model $model=null, $name)
	{
		$table = static::table();

		if (($rel = $table->get_relationship($name)))
		{
			if ($rel->is_poly())
			{
				// if the related model is null and it is a poly then we should have an empty array
				if (is_null($model))
					return $this->__relationships[$name] = array();
				else
					return $this->__relationships[$name][] = $model;
			}
			else
				return $this->__relationships[$name] = $model;
		}

		throw new RelationshipException("Relationship named $name has not been declared for class: {$table->class->getName()}");
	}

	/**
	 * Reloads the attributes and relationships of this object from the database.
	 *
	 * @return Model
	 */
	public function reload()
	{
		$this->__relationships = array();
		$pk = array_values($this->get_values_for($this->get_primary_key()));

		$this->set_attributes_via_mass_assignment($this->find($pk)->attributes, false);
		$this->reset_dirty();

		return $this;
	}

	public function __clone()
	{
		$this->__relationships = array();
		$this->reset_dirty();
		return $this;
	}

	/**
	 * Resets the dirty array.
	 *
	 * @see dirty_attributes
	 */
	public function reset_dirty()
	{
		$this->__dirty = null;
	}

	/**
	 * A list of valid finder options.
	 *
	 * @var array
	 */
	static $VALID_OPTIONS = array('conditions', 'limit', 'offset', 'order', 'select', 'joins', 'include', 'readonly', 'group', 'from', 'having');

	/**
	 * Enables the use of dynamic finders.
	 *
	 * Dynamic finders are just an easy way to do queries quickly without having to
	 * specify an options array with conditions in it.
	 *
	 * <code>
	 * SomeModel::find_by_first_name('Tito');
	 * SomeModel::find_by_first_name_and_last_name('Tito','the Grief');
	 * SomeModel::find_by_first_name_or_last_name('Tito','the Grief');
	 * SomeModel::find_all_by_last_name('Smith');
	 * SomeModel::count_by_name('Bob')
	 * SomeModel::count_by_name_or_state('Bob','VA')
	 * SomeModel::count_by_name_and_state('Bob','VA')
	 * </code>
	 *
	 * You can also create the model if the find call returned no results:
	 *
	 * <code>
	 * Person::find_or_create_by_name('Tito');
	 *
	 * # would be the equivalent of
	 * if (!Person::find_by_name('Tito'))
	 *   Person::create(array('Tito'));
	 * </code>
	 *
	 * Some other examples of find_or_create_by:
	 *
	 * <code>
	 * Person::find_or_create_by_name_and_id('Tito',1);
	 * Person::find_or_create_by_name_and_id(array('name' => 'Tito', 'id' => 1));
	 * </code>
	 *
	 * @param string $method Name of method
	 * @param mixed $args Method args
	 * @return Model
	 * @throws {@link ActiveRecordException} if invalid query
	 * @see find
	 */
	public static function __callStatic($method, $args)
	{
		$options = static::extract_and_validate_options($args);
		$create = false;

		if (substr($method,0,17) == 'find_or_create_by')
		{
			$attributes = substr($method,17);

			// can't take any finders with OR in it when doing a find_or_create_by
			if (strpos($attributes,'_or_') !== false)
				throw new ActiveRecordException("Cannot use OR'd attributes in find_or_create_by");

			$create = true;
			$method = 'find_by' . substr($method,17);
		}

		if (substr($method,0,7) === 'find_by')
		{
			$attributes = substr($method,8);
			$options['conditions'] = SQLBuilder::create_conditions_from_underscored_string(static::connection(),$attributes,$args,static::$alias_attribute);

			if (!($ret = static::find('first',$options)) && $create)
				return static::create(SQLBuilder::create_hash_from_underscored_string($attributes,$args,static::$alias_attribute));

			return $ret;
		}
		elseif (substr($method,0,11) === 'find_all_by')
		{
			$options['conditions'] = SQLBuilder::create_conditions_from_underscored_string(static::connection(),substr($method,12),$args,static::$alias_attribute);
			return static::find('all',$options);
		}
		elseif (substr($method,0,8) === 'count_by')
		{
			$options['conditions'] = SQLBuilder::create_conditions_from_underscored_string(static::connection(),substr($method,9),$args,static::$alias_attribute);
			return static::count($options);
		}

		throw new ActiveRecordException("Call to undefined method: $method");
	}

	/**
	 * Enables the use of build|create for associations.
	 *
	 * @param string $method Name of method
	 * @param mixed $args Method args
	 * @return mixed An instance of a given {@link AbstractRelationship}
	 */
	public function __call($method, $args)
	{
		//check for build|create_association methods
		if (preg_match('/(build|create)_/', $method))
		{
			if (!empty($args))
				$args = $args[0];

			$association_name = str_replace(array('build_', 'create_'), '', $method);
			$method = str_replace($association_name, 'association', $method);
			$table = static::table();

			if (($association = $table->get_relationship($association_name)) ||
				  ($association = $table->get_relationship(($association_name = Utils::pluralize($association_name)))))
			{
				// access association to ensure that the relationship has been loaded
				// so that we do not double-up on records if we append a newly created
				$this->$association_name;
				return $association->$method($this, $args);
			}
		}

		throw new ActiveRecordException("Call to undefined method: $method");
	}

	/**
	 * Alias for self::find('all').
	 *
	 * @see find
	 * @return array array of records found
	 */
	public static function all(/* ... */)
	{
		return call_user_func_array('static::find',array_merge(array('all'),func_get_args()));
	}

	/**
	 * Get a count of qualifying records.
	 *
	 * <code>
	 * YourModel::count(array('conditions' => 'amount > 3.14159265'));
	 * </code>
	 *
	 * @see find
	 * @return int Number of records that matched the query
	 */
	public static function count(/* ... */)
	{
		$args = func_get_args();
		$options = static::extract_and_validate_options($args);
		$options['select'] = 'COUNT(*)';

		if (!empty($args) && !is_null($args[0]) && !empty($args[0]))
		{
			if (is_hash($args[0]))
				$options['conditions'] = $args[0];
			else
				$options['conditions'] = call_user_func_array('static::pk_conditions',$args);
		}

		$table = static::table();
		$sql = $table->options_to_sql($options);
		$values = $sql->get_where_values();
		return static::connection()->query_and_fetch_one($sql->to_s(),$values);
	}

	/**
	 * Determine if a record exists.
	 *
	 * <code>
	 * SomeModel::exists(123);
	 * SomeModel::exists(array('conditions' => array('id=? and name=?', 123, 'Tito')));
	 * SomeModel::exists(array('id' => 123, 'name' => 'Tito'));
	 * </code>
	 *
	 * @see find
	 * @return boolean
	 */
	public static function exists(/* ... */)
	{
		return call_user_func_array('static::count',func_get_args()) > 0 ? true : false;
	}

	/**
	 * Alias for self::find('first').
	 *
	 * @see find
	 * @return Model The first matched record or null if not found
	 */
	public static function first(/* ... */)
	{
		return call_user_func_array('static::find',array_merge(array('first'),func_get_args()));
	}

	/**
	 * Alias for self::find('last')
	 *
	 * @see find
	 * @return Model The last matched record or null if not found
	 */
	public static function last(/* ... */)
	{
		return call_user_func_array('static::find',array_merge(array('last'),func_get_args()));
	}

	/**
	 * Find records in the database.
	 *
	 * Finding by the primary key:
	 *
	 * <code>
	 * # queries for the model with id=123
	 * YourModel::find(123);
	 *
	 * # queries for model with id in(1,2,3)
	 * YourModel::find(1,2,3);
	 *
	 * # finding by pk accepts an options array
	 * YourModel::find(123,array('order' => 'name desc'));
	 * </code>
	 *
	 * Finding by using a conditions array:
	 *
	 * <code>
	 * YourModel::find('first', array('conditions' => array('name=?','Tito'),
	 *   'order' => 'name asc'))
	 * YourModel::find('all', array('conditions' => 'amount > 3.14159265'));
	 * YourModel::find('all', array('conditions' => array('id in(?)', array(1,2,3))));
	 * </code>
	 *
	 * Finding by using a hash:
	 *
	 * <code>
	 * YourModel::find(array('name' => 'Tito', 'id' => 1));
	 * YourModel::find('first',array('name' => 'Tito', 'id' => 1));
	 * YourModel::find('all',array('name' => 'Tito', 'id' => 1));
	 * </code>
	 *
	 * An options array can take the following parameters:
	 *
	 * <ul>
	 * <li><b>select:</b> A SQL fragment for what fields to return such as: '*', 'people.*', 'first_name, last_name, id'</li>
	 * <li><b>joins:</b> A SQL join fragment such as: 'JOIN roles ON(roles.user_id=user.id)' or a named association on the model</li>
	 * <li><b>include:</b> TODO not implemented yet</li>
	 * <li><b>conditions:</b> A SQL fragment such as: 'id=1', array('id=1'), array('name=? and id=?','Tito',1), array('name IN(?)', array('Tito','Bob')),
	 * array('name' => 'Tito', 'id' => 1)</li>
	 * <li><b>limit:</b> Number of records to limit the query to</li>
	 * <li><b>offset:</b> The row offset to return results from for the query</li>
	 * <li><b>order:</b> A SQL fragment for order such as: 'name asc', 'name asc, id desc'</li>
	 * <li><b>readonly:</b> Return all the models in readonly mode</li>
	 * <li><b>group:</b> A SQL group by fragment</li>
	 * </ul>
	 *
	 * @throws {@link RecordNotFound} if no options are passed or finding by pk and no records matched
	 * @return mixed An array of records found if doing a find_all otherwise a
	 *   single Model object or null if it wasn't found. NULL is only return when
	 *   doing a first/last find. If doing an all find and no records matched this
	 *   will return an empty array.
	 */
	public static function find(/* $type, $options */)
	{
		$class = get_called_class();

		if (func_num_args() <= 0)
			throw new RecordNotFound("Couldn't find $class without an ID");

		$args = func_get_args();
		$options = static::extract_and_validate_options($args);
		$num_args = count($args);
		$single = true;

		if ($num_args > 0 && ($args[0] === 'all' || $args[0] === 'first' || $args[0] === 'last'))
		{
			switch ($args[0])
			{
				case 'all':
					$single = false;
					break;

			 	case 'last':
					if (!array_key_exists('order',$options))
						$options['order'] = join(' DESC, ',static::table()->pk) . ' DESC';
					else
						$options['order'] = SQLBuilder::reverse_order($options['order']);

					// fall thru

			 	case 'first':
			 		$options['limit'] = 1;
			 		$options['offset'] = 0;
			 		break;
			}

			$args = array_slice($args,1);
			$num_args--;
		}
		//find by pk
		elseif (1 === count($args) && 1 == $num_args)
			$args = $args[0];

		// anything left in $args is a find by pk
		if ($num_args > 0 && !isset($options['conditions']))
			return static::find_by_pk($args, $options);

		$options['mapped_names'] = static::$alias_attribute;
		$list = static::table()->find($options);

		return $single ? (!empty($list) ? $list[0] : null) : $list;
	}

	/**
	 * Finder method which will find by a single or array of primary keys for this model.
	 *
	 * @see find
	 * @param array $values An array containing values for the pk
	 * @param array $options An options array
	 * @return Model
	 * @throws {@link RecordNotFound} if a record could not be found
	 */
	public static function find_by_pk($values, $options)
	{
		$options['conditions'] = static::pk_conditions($values);
		$list = static::table()->find($options);
		$results = count($list);

		if ($results != ($expected = count($values)))
		{
			$class = get_called_class();

			if ($expected == 1)
			{
				if (!is_array($values))
					$values = array($values);

				throw new RecordNotFound("Couldn't find $class with ID=" . join(',',$values));
			}

			$values = join(',',$values);
			throw new RecordNotFound("Couldn't find all $class with IDs ($values) (found $results, but was looking for $expected)");
		}
		return $expected == 1 ? $list[0] : $list;
	}

	/**
	 * Find using a raw SELECT query.
	 *
	 * <code>
	 * YourModel::find_by_sql("SELECT * FROM people WHERE name=?",array('Tito'));
	 * YourModel::find_by_sql("SELECT * FROM people WHERE name='Tito'");
	 * </code>
	 *
	 * @param string $sql The raw SELECT query
	 * @param array $values An array of values for any parameters that needs to be bound
	 * @return array An array of models
	 */
	public static function find_by_sql($sql, $values=null)
	{
		return static::table()->find_by_sql($sql, $values, true);
	}

	/**
	 * Helper method to run arbitrary queries against the model's database connection.
	 *
	 * @param string $sql SQL to execute
	 * @param array $values Bind values, if any, for the query
	 * @return object A PDOStatement object
	 */
	public static function query($sql, $values=null)
	{
		return static::connection()->query($sql, $values);
	}

	/**
	 * Determines if the specified array is a valid ActiveRecord options array.
	 *
	 * @param array $array An options array
	 * @param bool $throw True to throw an exception if not valid
	 * @return boolean True if valid otherwise valse
	 * @throws {@link ActiveRecordException} if the array contained any invalid options
	 */
	public static function is_options_hash($array, $throw=true)
	{
		if (is_hash($array))
		{
			$keys = array_keys($array);
			$diff = array_diff($keys,self::$VALID_OPTIONS);

			if (!empty($diff) && $throw)
				throw new ActiveRecordException("Unknown key(s): " . join(', ',$diff));

			$intersect = array_intersect($keys,self::$VALID_OPTIONS);

			if (!empty($intersect))
				return true;
		}
		return false;
	}

	/**
	 * Returns a hash containing the names => values of the primary key.
	 *
	 * @internal This needs to eventually support composite keys.
	 * @param mixed $args Primary key value(s)
	 * @return array An array in the form array(name => value, ...)
	 */
	public static function pk_conditions($args)
	{
		$table = static::table();
		$ret = array($table->pk[0] => $args);
		return $ret;
	}

	/**
	 * Pulls out the options hash from $array if any.
	 *
	 * @internal DO NOT remove the reference on $array.
	 * @param array &$array An array
	 * @return array A valid options array
	 */
	public static function extract_and_validate_options(array &$array)
	{
		$options = array();

		if ($array)
		{
			$last = &$array[count($array)-1];

			try
			{
				if (self::is_options_hash($last))
				{
					array_pop($array);
					$options = $last;
				}
			}
			catch (ActiveRecordException $e)
			{
				if (!is_hash($last))
					throw $e;

				$options = array('conditions' => $last);
			}
		}
		return $options;
	}

	/**
	 * Returns a JSON representation of this model.
	 *
	 * @see Serialization
	 * @param array $options An array containing options for json serialization (see {@link Serialization} for valid options)
	 * @return string JSON representation of the model
	 */
	public function to_json(array $options=array())
	{
		return $this->serialize('Json', $options);
	}

	/**
	 * Returns an XML representation of this model.
	 *
	 * @see Serialization
	 * @param array $options An array containing options for xml serialization (see {@link Serialization} for valid options)
	 * @return string XML representation of the model
	 */
	public function to_xml(array $options=array())
	{
		return $this->serialize('Xml', $options);
	}

   /**
   * Returns an CSV representation of this model.
   * Can take optional delimiter and enclosure
   * (defaults are , and double quotes)
   *
   * Ex:
   * <code>
   * ActiveRecord\CsvSerializer::$delimiter=';';
   * ActiveRecord\CsvSerializer::$enclosure='';
   * YourModel::find('first')->to_csv(array('only'=>array('name','level')));
   * returns: Joe,2
   *
   * YourModel::find('first')->to_csv(array('only_header'=>true,'only'=>array('name','level')));
   * returns: name,level
   * </code>
   *
   * @see Serialization
   * @param array $options An array containing options for csv serialization (see {@link Serialization} for valid options)
   * @return string CSV representation of the model
   */
  public function to_csv(array $options=array())
  {
    return $this->serialize('Csv', $options);
  }

	/**
	 * Returns an Array representation of this model.
	 *
	 * @see Serialization
	 * @param array $options An array containing options for json serialization (see {@link Serialization} for valid options)
	 * @return array Array representation of the model
	 */
	public function to_array(array $options=array())
	{
		return $this->serialize('Array', $options);
	}

	/**
	 * Creates a serializer based on pre-defined to_serializer()
	 *
	 * An options array can take the following parameters:
	 *
	 * <ul>
	 * <li><b>only:</b> a string or array of attributes to be included.</li>
	 * <li><b>excluded:</b> a string or array of attributes to be excluded.</li>
	 * <li><b>methods:</b> a string or array of methods to invoke. The method's name will be used as a key for the final attributes array
	 * along with the method's returned value</li>
	 * <li><b>include:</b> a string or array of associated models to include in the final serialized product.</li>
	 * </ul>
	 *
	 * @param string $type Either Xml, Json, Csv or Array
	 * @param array $options Options array for the serializer
	 * @return string Serialized representation of the model
	 */
	private function serialize($type, $options)
	{
		require_once 'Serialization.php';
		$class = "ActiveRecord\\{$type}Serializer";
		$serializer = new $class($this, $options);
		return $serializer->to_s();
	}

	/**
	 * Invokes the specified callback on this model.
	 *
	 * @param string $method_name Name of the call back to run.
	 * @param boolean $must_exist Set to true to raise an exception if the callback does not exist.
	 * @return boolean True if invoked or null if not
	 */
	private function invoke_callback($method_name, $must_exist=true)
	{
		return static::table()->callback->invoke($this,$method_name,$must_exist);
	}

	/**
	 * Executes a block of code inside a database transaction.
	 *
	 * <code>
	 * YourModel::transaction(function()
	 * {
	 *   YourModel::create(array("name" => "blah"));
	 * });
	 * </code>
	 *
	 * If an exception is thrown inside the closure the transaction will
	 * automatically be rolled back. You can also return false from your
	 * closure to cause a rollback:
	 *
	 * <code>
	 * YourModel::transaction(function()
	 * {
	 *   YourModel::create(array("name" => "blah"));
	 *   throw new Exception("rollback!");
	 * });
	 *
	 * YourModel::transaction(function()
	 * {
	 *   YourModel::create(array("name" => "blah"));
	 *   return false; # rollback!
	 * });
	 * </code>
	 *
	 * @param Closure $closure The closure to execute. To cause a rollback have your closure return false or throw an exception.
	 * @return boolean True if the transaction was committed, False if rolled back.
	 */
	public static function transaction($closure)
	{
		$connection = static::connection();

		try
		{
			$connection->transaction();

			if ($closure() === false)
			{
				$connection->rollback();
				return false;
			}
			else
				$connection->commit();
		}
		catch (\Exception $e)
		{
			$connection->rollback();
			throw $e;
		}
		return true;
	}
};
?>
