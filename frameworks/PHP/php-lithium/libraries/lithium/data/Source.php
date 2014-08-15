<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data;

use lithium\core\NetworkException;

/**
 * This is the base class for Lithium's data abstraction layer.
 *
 * In addition to utility methods and standardized properties, it defines the implementation tasks
 * for all Lithium classes that work with external data, such as connections to remote resources
 * (`connect()` and `disconnect()`), introspecting available data objects (`sources()` and
 * `describe()`), and a standard read/write interface (`create()`, `read()`, `update()` and
 * `delete()`).
 *
 * Subclasses may implement any other non-standard functionality, but the above methods define the
 * requirements for interacting with `Model` objects, and other classes within `lithium\data`.
 */
abstract class Source extends \lithium\core\Object {

	/**
	 * The list of object properties to be automatically assigned from configuration passed to
	 * `__construct()`.
	 *
	 * @var array
	 */
	protected $_autoConfig = array('classes' => 'merge');

	/**
	 * Default entity and set classes used by subclasses of `Source`.
	 *
	 * @var array
	 */
	protected $_classes = array(
		'entity' => 'lithium\data\Entity',
		'set' => 'lithium\data\Collection',
		'relationship' => 'lithium\data\model\Relationship'
	);

	/**
	 * Stores a connection to a remote resource. Usually a database connection (`resource` type),
	 * or an HTTP connection object ('object' type).
	 *
	 * @var mixed
	 */
	public $connection = null;

	/**
	 * Stores the status of this object's connection. Updated when `connect()` or `disconnect()` are
	 * called, or if an error occurs that closes the object's connection.
	 *
	 * @var boolean
	 */
	protected $_isConnected = false;

	/**
	 * Constructor. Sets defaults and returns object.
	 *
	 * Options defined:
	 * - 'autoConnect' `boolean` If true, a connection is made on initialization. Defaults to true.
	 *
	 * @param array $config
	 * @return Source object
	 */
	public function __construct(array $config = array()) {
		$defaults = array('autoConnect' => true);
		parent::__construct($config + $defaults);
	}

	/**
	 * Ensures the connection is closed, before the object is destroyed.
	 *
	 * @return void
	 */
	public function __destruct() {
		if ($this->isConnected()) {
			$this->disconnect();
		}
	}

	protected function _init() {
		parent::_init();
		if ($this->_config['autoConnect']) {
			$this->connect();
		}
	}

	/**
	 * Checks the connection status of this data source. If the `'autoConnect'` option is set to
	 * true and the source connection is not currently active, a connection attempt will be made
	 * before returning the result of the connection status.
	 *
	 * @param array $options The options available for this method:
	 *        - 'autoConnect': If true, and the connection is not currently active, calls
	 *        `connect()` on this object. Defaults to `false`.
	 * @return boolean Returns the current value of `$_isConnected`, indicating whether or not
	 *         the object's connection is currently active.  This value may not always be accurate,
	 *         as the connection could have timed out or otherwise been dropped by the remote
	 *         resource during the course of the request.
	 */
	public function isConnected(array $options = array()) {
		$defaults = array('autoConnect' => false);
		$options += $defaults;

		if (!$this->_isConnected && $options['autoConnect']) {
			try {
				$this->connect();
			} catch (NetworkException $e) {
				$this->_isConnected = false;
			}
		}
		return $this->_isConnected;
	}

	/**
	 * Quotes data-source-native identifiers, where applicable.
	 *
	 * @param string $name Identifier name.
	 * @return string Returns `$name`, quoted if applicable.
	 */
	public function name($name) {
		return $name;
	}

	/**
	 * Abstract. Must be defined by child classes.
	 */
	abstract public function connect();

	/**
	 * Abstract. Must be defined by child classes.
	 */
	abstract public function disconnect();

	/**
	 * Returns a list of objects (sources) that models can bind to, i.e. a list of tables in the
	 * case of a database, or REST collections, in the case of a web service.
	 *
	 * @param string $class The fully-name-spaced class name of the object making the request.
	 * @return array Returns an array of objects to which models can connect.
	 */
	abstract public function sources($class = null);

	/**
	 * Gets the column schema for a given entity (such as a database table).
	 *
	 * @param mixed $entity Specifies the table name for which the schema should be returned, or
	 *        the class name of the model object requesting the schema, in which case the model
	 *        class will be queried for the correct table name.
	 * @param array $schema
	 * @param array $meta The meta-information for the model class, which this method may use in
	 *        introspecting the schema.
	 * @return array Returns a `Schema` object describing the given model's schema, where the
	 *         array keys are the available fields, and the values are arrays describing each
	 *         field, containing the following keys:
	 *         - `'type'`: The field type name
	 */
	abstract public function describe($entity, $schema = array(), array $meta = array());

	/**
	 * Defines or modifies the default settings of a relationship between two models.
	 *
	 * @param $class the primary model of the relationship
	 * @param $type the type of the relationship (hasMany, hasOne, belongsTo)
	 * @param $name the name of the relationship
	 * @param array $options relationship options
	 * @return array Returns an array containing the configuration for a model relationship.
	 */
	abstract public function relationship($class, $type, $name, array $options = array());

	/**
	 * Create a record. This is the abstract method that is implemented by specific data sources.
	 * This method should take a query object and use it to create a record in the data source.
	 *
	 * @param mixed $query An object which defines the update operation(s) that should be performed
	 *        against the data store.  This can be a `Query`, a `RecordSet`, a `Record`, or a
	 *        subclass of one of the three. Alternatively, `$query` can be an adapter-specific
	 *        query string.
	 * @param array $options The options from Model include,
	 *              - `validate` _boolean_ default: true
	 *              - `events` _string_ default: create
	 *              - `whitelist` _array_ default: null
	 *              - `callbacks` _boolean_ default: true
	 *              - `locked` _boolean_ default: true
	 * @return boolean Returns true if the operation was a success, otherwise false.
	 */
	abstract public function create($query, array $options = array());

	/**
	 * Abstract. Must be defined by child classes.
	 *
	 * @param mixed $query
	 * @param array $options
	 * @return boolean Returns true if the operation was a success, otherwise false.
	 */
	abstract public function read($query, array $options = array());

	/**
	 * Updates a set of records in a concrete data store.
	 *
	 * @param mixed $query An object which defines the update operation(s) that should be performed
	 *        against the data store.  This can be a `Query`, a `RecordSet`, a `Record`, or a
	 *        subclass of one of the three. Alternatively, `$query` can be an adapter-specific
	 *        query string.
	 * @param array $options Options to execute, which are defined by the concrete implementation.
	 * @return boolean Returns true if the update operation was a success, otherwise false.
	 */
	abstract public function update($query, array $options = array());

	/**
	 * Abstract. Must be defined by child classes.
	 *
	 * @param mixed $query
	 * @param array $options
	 * @return boolean Returns true if the operation was a success, otherwise false.
	 */
	abstract public function delete($query, array $options = array());

	/**
	 * Casts data into proper format when added to a collection or entity object.
	 *
	 * @param mixed $entity The entity or collection for which data is being cast, or the name of
	 *              the model class to which the entity/collection is bound.
	 * @param array $data An array of data being assigned.
	 * @param array $options Any associated options with, for example, instantiating new objects in
	 *              which to wrap the data. Options implemented by `cast()` itself:
	 *              - `first` _boolean_: Used when only one value is passed to `cast()`. Even though
	 *                that value must be wrapped in an array, setting the `'first'` option to `true`
	 *                causes only that one value to be returned.
	 * @return mixed Returns the value of `$data`, cast to the proper format according to the schema
	 *         definition of the model class specified by `$model`.
	 */
	public function cast($entity, array $data, array $options = array()) {
		$defaults = array('first' => false);
		$options += $defaults;
		return $options['first'] ? reset($data) : $data;
	}

	/**
	 * Returns the list of methods which format values imported from `Query` objects. Should be
	 * overridden in subclasses.
	 *
	 * @see lithium\data\model\Query
	 * @return array
	 */
	public function methods() {
		return get_class_methods($this);
	}

	/**
	 * A method which can be optionally implemented to configure a model class.
	 *
	 * @see lithium\data\Model::$_meta
	 * @see lithium\data\Model::$_finders
	 * @see lithium\data\Model::$_classes
	 * @param string $class The name of the model class to be configured.
	 * @return array This method should return an array one or more of the following keys: `'meta'`,
	 *         `'classes'` or `'finders'`. These keys maps to the three corresponding properties in
	 *         `lithium\data\Model`, and are used to override the base-level default settings and
	 *         dependencies.
	 */
	public function configureClass($class) {
		return array('meta' => array('key' => 'id', 'locked' => true));
	}

	/**
	 * This method is responsible for factorying a new instance of a single entity object of correct
	 * type, matching the current data source class.
	 *
	 * @param string $model A fully-namespaced class name representing the model class to which the
	 *               `Entity` object will be bound.
	 * @param array $data The default data with which the new `Entity` should be populated.
	 * @param array $options Any additional options to pass to the `Entity`'s constructor
	 * @return object Returns a new, un-saved `Entity` object bound to the model class specified
	 *         in `$model`.
	 */
	public function item($model, array $data = array(), array $options = array()) {
		$defaults = array('class' => 'entity');
		$options += $defaults;

		$class = $options['class'];
		unset($options['class']);
		return $this->_instance($class, compact('model', 'data') + $options);
	}

	/**
	 * Applying a strategy to a `lithium\data\model\Query` object
	 *
	 * @param array $options The option array
	 * @param object $context A query object to configure
	 */
	public function applyStrategy($options, $context) {}
}

?>