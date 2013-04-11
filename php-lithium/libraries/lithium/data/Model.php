<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data;

use lithium\core\Libraries;
use lithium\util\Set;
use lithium\util\Inflector;
use lithium\core\ConfigException;
use BadMethodCallException;

/**
 * The `Model` class is the starting point for the domain logic of your application.
 * Models are tasked with providing meaning to otherwise raw and unprocessed data (e.g.
 * user profile).
 *
 * Models expose a consistent and unified API to interact with an underlying datasource (e.g.
 * MongoDB, CouchDB, MySQL) for operations such as querying, saving, updating and deleting data
 * from the persistent storage.
 *
 * Models allow you to interact with your data in two fundamentally different ways: querying, and
 * data mutation (saving/updating/deleting). All query-related operations may be done through the
 * static `find()` method, along with some additional utility methods provided for convenience.
 *
 * Classes extending this one should, conventionally, be named as Plural, CamelCase and be
 * placed in the `models` directory. i.e. a posts model would be `model/Posts.php`.
 *
 * Examples:
 * {{{
 * // Return all 'post' records
 * Posts::find('all');
 * Posts::all();
 *
 * // With conditions and a limit
 * Posts::find('all', array('conditions' => array('published' => true), 'limit' => 10));
 * Posts::all(array('conditions' => array('published' => true), 'limit' => 10));
 *
 * // Integer count of all 'post' records
 * Posts::find('count');
 * Posts::count(); // This is equivalent to the above.
 *
 * // With conditions
 * Posts::find('count', array('conditions' => array('published' => true)));
 * Posts::count(array('published' => true));
 * }}}
 *
 * The actual objects returned from `find()` calls will depend on the type of data source in use.
 * MongoDB, for example, will return results as a `Document` (as will CouchDB), while MySQL will
 * return results as a `RecordSet`. Both of these classes extend a common `lithium\data\Collection`
 * class, and provide the necessary abstraction to make working with either type completely
 * transparent.
 *
 * For data mutation (saving/updating/deleting), the `Model` class acts as a broker to the proper
 * objects. When creating a new record or document, for example, a call to `Posts::create()` will
 * return an instance of `lithium\data\entity\Record` or `lithium\data\entity\Document`, which can
 * then be acted upon.
 *
 * Example:
 * {{{
 * $post = Posts::create();
 * $post->author = 'Robert';
 * $post->title = 'Newest Post!';
 * $post->content = 'Lithium rocks. That is all.';
 *
 * $post->save();
 * }}}
 *
 * @see lithium\data\entity\Record
 * @see lithium\data\entity\Document
 * @see lithium\data\collection\RecordSet
 * @see lithium\data\collection\DocumentSet
 * @see lithium\data\Connections
 */
class Model extends \lithium\core\StaticObject {

	/**
	 * Criteria for data validation.
	 *
	 * Example usage:
	 * {{{
	 * public $validates = array(
	 *     'title' => 'please enter a title',
	 *     'email' => array(
	 *         array('notEmpty', 'message' => 'Email is empty.'),
	 *         array('email', 'message' => 'Email is not valid.'),
	 *     )
	 * );
	 * }}}
	 *
	 * @var array
	 */
	public $validates = array();

	/**
	 * Model hasOne relations.
	 * Not yet implemented.
	 *
	 * @var array
	 */
	public $hasOne = array();

	/**
	 * Model hasMany relations.
	 * Not yet implemented.
	 *
	 * @var array
	 */
	public $hasMany = array();

	/**
	 * Model belongsTo relations.
	 * Not yet implemented.
	 *
	 * @var array
	 */
	public $belongsTo = array();

	/**
	 * Stores model instances for internal use.
	 *
	 * While the `Model` public API does not require instantiation thanks to late static binding
	 * introduced in PHP 5.3, LSB does not apply to class attributes. In order to prevent you
	 * from needing to redeclare every single `Model` class attribute in subclasses, instances of
	 * the models are stored and used internally.
	 *
	 * @var array
	 */
	protected static $_instances = array();

	/**
	 * List of initialized instances.
	 *
	 * @see lithium\data\Model::_initialize();
	 * @var array
	 */
	protected static $_initialized = array();

	/**
	 * Stores the filters that are applied to the model instances stored in `Model::$_instances`.
	 *
	 * @var array
	 */
	protected $_instanceFilters = array();

	/**
	 * Class dependencies.
	 *
	 * @var array
	 */
	protected $_classes = array(
		'connections' => 'lithium\data\Connections',
		'query'       => 'lithium\data\model\Query',
		'validator'   => 'lithium\util\Validator',
		'entity'      => 'lithium\data\Entity'
	);

	/**
	 * A list of the current relation types for this `Model`.
	 *
	 * @var array
	 */
	protected $_relations = array();

	/**
	 * List of relation types.
	 *
	 * Valid relation types are:
	 *
	 * - `belongsTo`
	 * - `hasOne`
	 * - `hasMany`
	 *
	 * @var array
	 */
	protected $_relationTypes = array('belongsTo', 'hasOne', 'hasMany');

	/**
	 * Store available relation names for this model which still unloaded.
	 *
	 * @var array This array use the following notation : `relation_name => relation_type`.
	 */
	protected $_relationsToLoad = array();

	/**
	 * Specifies all meta-information for this model class, including the name of the data source it
	 * connects to, how it interacts with that class, and how its data structure is defined.
	 *
	 * - `connection`: The name of the connection (as defined in `Connections::add()`) to which the
	 *   model should bind
	 * - `key`: The primary key or identifier key for records / documents this model produces,
	 *   i.e. `'id'` or `array('_id', '_rev')`. Defaults to `'id'`.
	 * - `name`: The canonical name of this model. Defaults to the class name.
	 * - `source`: The name of the database table or document collection to bind to. Defaults to the
	 *   lower-cased and underscored name of the class, i.e. `class UserProfile` maps to
	 *   `'user_profiles'`.
	 * - `title`: The field or key used as the title for each record. Defaults to `'title'` or
	 *   `'name'`, if those fields are available.
	 *
	 * @var array
	 * @see lithium\data\Connections::add()
	 */
	protected $_meta = array(
		'name' => null,
		'title' => null,
		'class' => null,
		'source' => null,
		'connection' => 'default'
	);

	/**
	 * Array of closures used to lazily initialize metadata.
	 *
	 * @var array
	 */
	protected $_initializers = array();

	/**
	 * Stores the data schema.
	 *
	 * The schema is lazy-loaded by the first call to `Model::schema()`, unless it has been
	 * manually defined in the `Model` subclass.
	 *
	 * For schemaless persistent storage (e.g. MongoDB), this is never populated automatically - if
	 * you desire a fixed schema to interact with in those cases, you will be required to define it
	 * yourself.
	 *
	 * Example:
	 * {{{
	 * protected $_schema = array(
	 *     '_id'  => array('type' => 'id'), // required for Mongo
	 *     'name' => array('type' => 'string', 'default' => 'Moe', 'null' => false),
	 *     'sign' => array('type' => 'string', 'default' => 'bar', 'null' => false),
	 *     'age'  => array('type' => 'integer', 'default' => 0, 'null' => false)
	 * );
	 * }}}
	 *
	 * For MongoDB specifically, you can also implement a callback in your database connection
	 * configuration that fetches and returns the schema data, as in the following:
	 *
	 * {{{
	 * // config/bootstrap/connections.php:
	 * Connections::add('default', array(
	 * 	'type' => 'MongoDb',
	 * 	'host' => 'localhost',
	 * 	'database' => 'app_name',
	 * 	'schema' => function($db, $collection, $meta) {
	 * 		$result = $db->connection->schemas->findOne(compact('collection'));
	 * 		return $result ? $result['data'] : array();
	 * 	}
	 * ));
	 * }}}
	 *
	 * This example defines an optional MongoDB convention in which the schema for each individual
	 * collection is stored in a "schemas" collection, where each document contains the name of
	 * a collection, along with a `'data'` key, which contains the schema for that collection, in
	 * the format specified above.
	 *
	 * When defining `'$_schema'` where the data source is MongoDB, the types map to database
	 * types as follows:
	 *
	 * {{{
	 *	id      => MongoId
	 *	date    => MongoDate
	 *	regex   => MongoRegex
	 *	integer => integer
	 *	float   => float
	 *	boolean => boolean
	 *	code    => MongoCode
	 *	binary  => MongoBinData
	 * }}}
	 *
	 * @see lithium\data\source\MongoDb::$_schema
	 * @var array
	 */
	protected $_schema = array();

	/**
	 * Default query parameters.
	 *
	 * - `'conditions'`: The conditional query elements, e.g.
	 *                 `'conditions' => array('published' => true)`
	 * - `'fields'`: The fields that should be retrieved. When set to `null`, defaults to
	 *             all fields.
	 * - `'order'`: The order in which the data will be returned, e.g. `'order' => 'ASC'`.
	 * - `'limit'`: The maximum number of records to return.
	 * - `'page'`: For pagination of data.
	 * - `'with'`: An array of relationship names to be included in the query.
	 *
	 * @var array
	 */
	protected $_query = array(
		'conditions' => null,
		'fields'     => null,
		'order'      => null,
		'limit'      => null,
		'page'       => null,
		'with'       => array()
	);

	/**
	 * Custom find query properties, indexed by name.
	 *
	 * @see lithium\data\Model::finder()
	 * @var array
	 */
	protected $_finders = array();

	/**
	 * Stores all custom instance methods created by `Model::instanceMethods`.
	 *
	 * @var array
	 */
	protected static $_instanceMethods = array();

	/**
	 * Holds an array of values that should be processed on `Model::config()`. Each value should
	 * have a matching protected property (prefixed with `_`) defined in the class. If the
	 * property is an array, the property name should be the key and the value should be `'merge'`.
	 *
	 * @see lithium\data\Model::config()
	 * @var array
	 */
	protected $_autoConfig = array(
		'meta',
		'finders',
		'query',
		'schema',
		'classes',
		'initializers'
	);

	/**
	 * Configures the model for use. This method will set the `Model::$_schema`, `Model::$_meta`,
	 * `Model::$_finders` class attributes, as well as obtain a handle to the configured
	 * persistent storage connection.
	 *
	 * @param array $config Possible options are:
	 *        - `meta`: Meta-information for this model, such as the connection.
	 *        - `finders`: Custom finders for this model.
	 *        - `query`: Default query parameters.
	 *        - `schema`: A `Schema` instance for this model.
	 *        - `classes`: Classes used by this model.
	 */
	public static function config(array $config = array()) {
		if (($class = get_called_class()) === __CLASS__) {
			return;
		}

		if (!isset(static::$_instances[$class])) {
			static::$_instances[$class] = new $class();
		}
		$self = static::$_instances[$class];

		foreach ($self->_autoConfig as $key) {
			if (isset($config[$key])) {
				$_key = "_{$key}";
				$val = $config[$key];
				$self->$_key = is_array($val) ? $val + $self->$_key : $val;
			}
		}

		static::$_initialized[$class] = false;
	}

	/**
	 * Init default connection options and connects default finders.
	 *
	 * This method will set the `Model::$_schema`, `Model::$_meta`, `Model::$_finders` class
	 * attributes, as well as obtain a handle to the configured persistent storage connection
	 *
	 * @param string $class The fully-namespaced class name to initialize.
	 * @return object Returns the initialized model instance.
	 */
	protected static function _initialize($class) {
		$self = static::$_instances[$class];

		if (isset(static::$_initialized[$class]) && static::$_initialized[$class]) {
			return $self;
		}
		static::$_initialized[$class] = true;

		$query   = array();
		$finders = array();
		$meta    = array();
		$schema  = array();
		$source  = array();
		$classes = $self->_classes;
		$initializers = array();

		foreach (static::_parents() as $parent) {
			$parentConfig = get_class_vars($parent);

			foreach ($self->_autoConfig as $key) {
				if (isset($parentConfig["_{$key}"])) {
					$val = $parentConfig["_{$key}"];
					${$key} = is_array($val) ? ${$key} + $val : $val;
				}
			}
			if ($parent === __CLASS__) {
				break;
			}
		}

		$tmp = $self->_meta + $meta;
		$source = array('meta' => array(), 'finders' => array(), 'schema' => array());

		if ($tmp['connection']) {
			$conn = $classes['connections']::get($tmp['connection']);
			$source = (($conn) ? $conn->configureClass($class) : array()) + $source;
		}
		$self->_classes = $classes;

		$local = compact('class') + $self->_meta;
		$self->_meta = ($local + $source['meta'] + $meta);

		$self->_initializers += array(
			'name' => function($self) {
				return basename(str_replace('\\', '/', $self));
			},
			'source' => function($self) {
				return Inflector::tableize($self::meta('name'));
			},
			'title' => function($self) {
				$titleKeys = array('title', 'name');
				$titleKeys = array_merge($titleKeys, (array) $self::meta('key'));
				return $self::hasField($titleKeys);
			}
		);

		if (is_object($self->_schema)) {
			$self->_schema->append($source['schema']);
		} elseif (is_array($self->_schema)) {
			$self->_schema = $self->_schema + $source['schema'];
		} else {
			$self->_schema = $source['schema'];
		}

		$self->_finders += $source['finders'] + $self->_findFilters();

		static::_relationsToLoad();
		return $self;
	}

	/**
	 * Returns an instance of a class with given `config`. The `name` could be a key from the
	 * `classes` array, a fully-namespaced class name, or an object. Typically this method is used
	 * in `_init` to create the dependencies used in the current class.
	 *
	 * @param string|object $name A `classes` alias or fully-namespaced class name.
	 * @param array $options The configuration passed to the constructor.
	 * @return object
	 */
	protected static function _instance($name, array $options = array()) {
		$self = static::_object();
		if (is_string($name) && isset($self->_classes[$name])) {
			$name = $self->_classes[$name];
		}
		return Libraries::instance(null, $name, $options);
	}

	/**
	 * Allows the use of syntactic-sugar like `Model::all()` instead of `Model::find('all')`.
	 *
	 * @see lithium\data\Model::find()
	 * @see lithium\data\Model::$_meta
	 * @link http://php.net/manual/en/language.oop5.overloading.php PHP Manual: Overloading
	 *
	 * @throws BadMethodCallException On unhandled call, will throw an exception.
	 * @param string $method Method name caught by `__callStatic()`.
	 * @param array $params Arguments given to the above `$method` call.
	 * @return mixed Results of dispatched `Model::find()` call.
	 */
	public static function __callStatic($method, $params) {
		$self = static::_object();
		$isFinder = isset($self->_finders[$method]);

		if ($isFinder && count($params) === 2 && is_array($params[1])) {
			$params = array($params[1] + array($method => $params[0]));
		}

		if ($method === 'all' || $isFinder) {
			if ($params && !is_array($params[0])) {
				$params[0] = array('conditions' => static::key($params[0]));
			}
			return $self::find($method, $params ? $params[0] : array());
		}
		preg_match('/^findBy(?P<field>\w+)$|^find(?P<type>\w+)By(?P<fields>\w+)$/', $method, $args);

		if (!$args) {
			$message = "Method `%s` not defined or handled in class `%s`.";
			throw new BadMethodCallException(sprintf($message, $method, get_class($self)));
		}

		$field = Inflector::underscore($args['field'] ? $args['field'] : $args['fields']);
		$type = isset($args['type']) ? $args['type'] : 'first';
		$type[0] = strtolower($type[0]);

		$conditions = array($field => array_shift($params));
		$params = (isset($params[0]) && count($params) === 1) ? $params[0] : $params;
		return $self::find($type, compact('conditions') + $params);
	}

	/**
	 * Magic method that allows calling `Model::_instanceMethods`'s closure like normal methods
	 * on the model instance.
	 *
	 * @see lithium\data\Model::instanceMethods
	 * @param string $method Method name caught by `__call()`.
	 * @param array $params Arguments given to the above `$method` call.
	 * @return mixed
	 */
	public function __call($method, $params) {
		$methods = static::instanceMethods();
		if (isset($methods[$method]) && is_callable($methods[$method])) {
			return call_user_func_array($methods[$method], $params);
		}
		$message = "Unhandled method call `{$method}`.";
		throw new BadMethodCallException($message);
	}

	/**
	 * Custom check to determine if our given magic methods can be responded to.
	 *
	 * @param  string  $method     Method name.
	 * @param  bool    $internal   Interal call or not.
	 * @return bool
	 */
	public static function respondsTo($method, $internal = false) {
		$self = static::_object();
		$methods = static::instanceMethods();
		$isFinder = isset($self->_finders[$method]);
		preg_match('/^findBy(?P<field>\w+)$|^find(?P<type>\w+)By(?P<fields>\w+)$/', $method, $args);
		$staticRepondsTo = $isFinder || $method === 'all' || !!$args;
		$instanceRespondsTo = isset($methods[$method]);
		return $instanceRespondsTo || $staticRepondsTo || parent::respondsTo($method, $internal);
	}

	/**
	 * The `find` method allows you to retrieve data from the connected data source.
	 *
	 * Examples:
	 * {{{
	 * Posts::find('all'); // returns all records
	 * Posts::find('count'); // returns a count of all records
	 *
	 * // The first ten records that have 'author' set to 'Lithium'
	 * Posts::find('all', array(
	 *     'conditions' => array('author' => "Bob"), 'limit' => 10
	 * ));
	 * }}}
	 *
	 * @see lithium\data\Model::$_finders
	 * @param string $type The find type, which is looked up in `Model::$_finders`. By default it
	 *        accepts `all`, `first`, `list` and `count`,
	 * @param array $options Options for the query. By default, accepts:
	 *        - `conditions`: The conditional query elements, e.g.
	 *                 `'conditions' => array('published' => true)`
	 *        - `fields`: The fields that should be retrieved. When set to `null`, defaults to
	 *             all fields.
	 *        - `order`: The order in which the data will be returned, e.g. `'order' => 'ASC'`.
	 *        - `limit`: The maximum number of records to return.
	 *        - `page`: For pagination of data.
	 * @return mixed
	 * @filter This method can be filtered.
	 */
	public static function find($type, array $options = array()) {
		$self = static::_object();
		$finder = array();

		if ($type === null) {
			return null;
		}
		$isFinder = is_string($type) && isset($self->_finders[$type]);

		if ($type !== 'all' && !is_array($type) && !$isFinder) {
			$options['conditions'] = static::key($type);
			$type = 'first';
		}

		if ($isFinder && is_array($self->_finders[$type])) {
			$options = Set::merge($self->_finders[$type], $options);
		}

		$options = (array) $options + (array) $self->_query;
		$meta = array('meta' => $self->_meta, 'name' => get_called_class());
		$params = compact('type', 'options');

		$filter = function($self, $params) use ($meta) {
			$options = $params['options'] + array('type' => 'read', 'model' => $meta['name']);
			$query = $self::invokeMethod('_instance', array('query', $options));
			return $self::connection()->read($query, $options);
		};
		if (is_string($type) && isset($self->_finders[$type])) {
			$finder = is_callable($self->_finders[$type]) ? array($self->_finders[$type]) : array();
		}
		return static::_filter(__FUNCTION__, $params, $filter, $finder);
	}

	/**
	 * Gets or sets a finder by name.  This can be an array of default query options,
	 * or a closure that accepts an array of query options, and a closure to execute.
	 *
	 * @param string $name The finder name, e.g. `first`.
	 * @param string $finder If you are setting a finder, this is the finder definition.
	 * @return mixed Returns finder definition if querying, or `null` if setting.
	 */
	public static function finder($name, $finder = null) {
		$self = static::_object();

		if (!$finder) {
			return isset($self->_finders[$name]) ? $self->_finders[$name] : null;
		}
		$self->_finders[$name] = $finder;
	}

	/**
	 * Gets or sets the default query for the model.
	 *
	 * @param array $query.  Possible options are:
	 *        - `'conditions'`: The conditional query elements, e.g.
	 *          `'conditions' => array('published' => true)`
	 *        - `'fields'`: The fields that should be retrieved. When set to `null`, defaults to
	 *          all fields.
	 *        - `'order'`: The order in which the data will be returned, e.g. `'order' => 'ASC'`.
	 *        - `'limit'`: The maximum number of records to return.
	 *        - `'page'`: For pagination of data.
	 *        - `'with'`: An array of relationship names to be included in the query.
	 *
	 * @return mixed Returns the query definition if querying, or `null` if setting.
	 */
	public static function query($query = null) {
		$self = static::_object();

		if (!$query) {
			return $self->_query;
		}
		$self->_query += $query;
	}

	/**
	 * Gets or sets Model's metadata.
	 *
	 * @see lithium\data\Model::$_meta
	 * @param string $key Model metadata key.
	 * @param string $value Model metadata value.
	 * @return mixed Metadata value for a given key.
	 */
	public static function meta($key = null, $value = null) {
		$self = static::_object();
		$isArray = is_array($key);

		if ($value || $isArray) {
			$value ? $self->_meta[$key] = $value : $self->_meta = $key + $self->_meta;
			return;
		}
		return $self->_getMetaKey($isArray ? null : $key);
	}

	/**
	 * Helper method used by `meta()` to generate and cache metadata values.
	 *
	 * @param string $key The name of the meta value to return, or `null`, to return all values.
	 * @return mixed Returns the value of the meta key specified by `$key`, or an array of all meta
	 *         values if `$key` is `null`.
	 */
	protected function _getMetaKey($key = null) {
		if (!$key) {
			$all = array_keys($this->_initializers);
			$call = array(&$this, '_getMetaKey');
			return $all ? array_combine($all, array_map($call, $all)) + $this->_meta : $this->_meta;
		}

		if (isset($this->_meta[$key])) {
			return $this->_meta[$key];
		}
		if (isset($this->_initializers[$key]) && $initializer = $this->_initializers[$key]) {
			unset($this->_initializers[$key]);
			return ($this->_meta[$key] = $initializer(get_called_class()));
		}
	}

	/**
	 * The `title()` method is invoked whenever an `Entity` object is cast or coerced
	 * to a string. This method can also be called on the entity directly, i.e. `$post->title()`.
	 *
	 * By default, when generating the title for an object, it uses the the field specified in
	 * the `'title'` key of the model's meta data definition. Override this method to generate
	 * custom titles for objects of this model's type.
	 *
	 * @see lithium\data\Model::$_meta
	 * @see lithium\data\Entity::__toString()
	 * @param object $entity The `Entity` instance on which the title method is called.
	 * @return string Returns the title representation of the entity on which this method is called.
	 */
	public function title($entity) {
		$field = static::meta('title');
		return $entity->{$field};
	}

	/**
	 * If no values supplied, returns the name of the `Model` key. If values
	 * are supplied, returns the key value.
	 *
	 * @param mixed $values An array of values or object with values. If `$values` is `null`,
	 *              the meta `'key'` of the model is returned.
	 * @return mixed Key value.
	 */
	public static function key($values = null) {
		$key = static::meta('key');

		if ($values === null) {
			return $key;
		}

		$self = static::_object();
		$entity = $self->_classes['entity'];
		if (is_object($values) && is_string($key)) {
			return static::_key($key, $values, $entity);
		} elseif ($values instanceof $entity) {
			$values = $values->to('array');
		}

		if (!is_array($values) && !is_array($key)) {
			return array($key => $values);
		}

		$key = (array) $key;
		$result = array();
		foreach ($key as $value) {
			if (!isset($values[$value])) {
				return null;
			}
			$result[$value] = $values[$value];
		}
		return $result;
	}

	/**
	 * Helper for the `Model::key()` function
	 *
	 * @see lithium\data\Model::key()
	 * @param string $key The key
	 * @param object $values Object with attributes.
	 * @param string $entity The fully-namespaced entity class name.
	 * @return mixed The key value array or `null` if the `$values` object has no attribute
	 *         named `$key`
	 */
	protected static function _key($key, $values, $entity) {
		if (isset($values->$key)) {
			return array($key => $values->$key);
		} elseif (!$values instanceof $entity) {
			return array($key => $values);
		}
		return null;
	}

	/**
	 * Returns a list of models related to `Model`, or a list of models related
	 * to this model, but of a certain type.
	 *
	 * @param string $type A type of model relation.
	 * @return mixed An array of relation instances or an instance of relation.
	 *
	 */
	public static function relations($type = null) {
		$self = static::_object();

		if ($type === null) {
			return static::_relations();
		}

		if (isset($self->_relations[$type])) {
			return $self->_relations[$type];
		}

		if (isset($self->_relationsToLoad[$type])) {
			return static::_relations(null, $type);
		}

		if (in_array($type, $self->_relationTypes, true)) {
			return array_keys(static::_relations($type));
		}
		return null;
	}

	/**
	 * This method automagically bind in the fly unloaded relations.
	 *
	 * @see lithium\data\model::relations()
	 * @param $type A type of model relation.
	 * @param $name A relation name.
	 * @return An array of relation instances or an instance of relation.
	 */
	protected static function _relations($type = null, $name = null) {
		$self = static::_object();

		if ($name) {
			if (isset($self->_relationsToLoad[$name])) {
				$t = $self->_relationsToLoad[$name];
				unset($self->_relationsToLoad[$name]);
				return static::bind($t, $name, (array) $self->{$t}[$name]);
			}
			return isset($self->_relations[$name]) ? $self->_relations[$name] : null;
		}
		if (!$type) {
			foreach ($self->_relationsToLoad as $name => $t) {
				static::bind($t, $name, (array) $self->{$t}[$name]);
			}
			$self->_relationsToLoad = array();
			return $self->_relations;
		}
		foreach ($self->_relationsToLoad as $name => $t) {
			if ($type === $t) {
				static::bind($t, $name, (array) $self->{$t}[$name]);
				unset($self->_relationsToLoad[$name]);
			}
		}
		return array_filter($self->_relations, function($i) use ($type) {
			return $i->data('type') === $type;
		});
	}


	/**
	 * Creates a relationship binding between this model and another.
	 *
	 * @see lithium\data\model\Relationship
	 * @param string $type The type of relationship to create. Must be one of `'hasOne'`,
	 *               `'hasMany'` or `'belongsTo'`.
	 * @param string $name The name of the relationship. If this is also the name of the model,
	 *               the model must be in the same namespace as this model. Otherwise, the
	 *               fully-namespaced path to the model class must be specified in `$config`.
	 * @param array $config Any other configuration that should be specified in the relationship.
	 *              See the `Relationship` class for more information.
	 * @return object Returns an instance of the `Relationship` class that defines the connection.
	 */
	public static function bind($type, $name, array $config = array()) {
		$self = static::_object();

		if (!in_array($type, $self->_relationTypes)) {
			throw new ConfigException("Invalid relationship type `{$type}` specified.");
		}
		$rel = static::connection()->relationship(get_called_class(), $type, $name, $config);
		return $self->_relations[$name] = $rel;
	}

	/**
	 * Lazy-initialize the schema for this Model object, if it is not already manually set in the
	 * object. You can declare `protected $_schema = array(...)` to define the schema manually.
	 *
	 * @param mixed $field Optional. You may pass a field name to get schema information for just
	 *        one field. Otherwise, an array containing all fields is returned. If `false`, the
	 *        schema is reset to an empty value. If an array, field definitions contained are
	 *        appended to the schema.
	 * @return array
	 */
	public static function schema($field = null) {
		$self = static::_object();

		if (!is_object($self->_schema)) {
			$self->_schema = static::connection()->describe(
				$self::meta('source'), $self->_schema, $self->_meta
			);
			if (!is_object($self->_schema)) {
				$class = get_called_class();
				throw new ConfigException("Could not load schema object for model `{$class}`.");
			}
			$key = (array) $self::meta('key');

			if ($self->_schema && $self->_schema->fields() && !$self->_schema->has($key)) {
				$key = implode('`, `', $key);
				throw new ConfigException("Missing key `{$key}` from schema.");
			}
		}
		if ($field === false) {
			return $self->_schema->reset();
		}
		if (is_array($field)) {
			return $self->_schema->append($field);
		}
		return $field ? $self->_schema->fields($field) : $self->_schema;
	}

	/**
	 * Checks to see if a particular field exists in a model's schema. Can check a single field, or
	 * return the first field found in an array of multiple options.
	 *
	 * @param mixed $field A single field (string) or list of fields (array) to check the existence
	 *        of.
	 * @return mixed If `$field` is a string, returns a boolean indicating whether or not that field
	 *         exists. If `$field` is an array, returns the first field found, or `false` if none of
	 *         the fields in the list are found.
	 */
	public static function hasField($field) {
		if (!is_array($field)) {
			return static::schema()->fields($field);
		}
		foreach ($field as $f) {
			if (static::hasField($f)) {
				return $f;
			}
		}
		return false;
	}

	/**
	 * Instantiates a new record or document object, initialized with any data passed in. For
	 * example:
	 *
	 * {{{
	 * $post = Posts::create(array('title' => 'New post'));
	 * echo $post->title; // echoes 'New post'
	 * $success = $post->save();
	 * }}}
	 *
	 * Note that while this method creates a new object, there is no effect on the database until
	 * the `save()` method is called.
	 *
	 * In addition, this method can be used to simulate loading a pre-existing object from the
	 * database, without actually querying the database:
	 *
	 * {{{
	 * $post = Posts::create(array('id' => $id, 'moreData' => 'foo'), array('exists' => true));
	 * $post->title = 'New title';
	 * $success = $post->save();
	 * }}}
	 *
	 * This will create an update query against the object with an ID matching `$id`. Also note that
	 * only the `title` field will be updated.
	 *
	 * @param array $data Any data that this object should be populated with initially.
	 * @param array $options Options to be passed to item.
	 * @return object Returns a new, _un-saved_ record or document object. In addition to the values
	 *         passed to `$data`, the object will also contain any values assigned to the
	 *         `'default'` key of each field defined in `$_schema`.
	 * @filter
	 */
	public static function create(array $data = array(), array $options = array()) {
		return static::_filter(__FUNCTION__, compact('data', 'options'), function($self, $params) {
			$data = Set::merge(Set::expand($self::schema()->defaults()), $params['data']);
			return $self::connection()->item($self, $data, $params['options']);
		});
	}

	/**
	 * Getter and setter for custom instance methods. This is used in `Entity::__call()`.
	 *
	 * {{{
	 * Model::instanceMethods(array(
	 *     'methodName' => array('Class', 'method'),
	 *     'anotherMethod' => array($object, 'method'),
	 *     'closureCallback' => function($entity) {}
	 * ));
	 * }}}
	 *
	 * @see lithium\data\Entity::__call()
	 * @param array $methods
	 * @return array
	 */
	public static function instanceMethods(array $methods = null) {
		$class = get_called_class();

		if (!isset(static::$_instanceMethods[$class])) {
			static::$_instanceMethods[$class] = array();
		}
		if ($methods === array()) {
			return static::$_instanceMethods[$class] = array();
		}
		if (!is_null($methods)) {
			static::$_instanceMethods[$class] = $methods + static::$_instanceMethods[$class];
		}
		return static::$_instanceMethods[$class];
	}

	/**
	 * An instance method (called on record and document objects) to create or update the record or
	 * document in the database that corresponds to `$entity`.
	 *
	 * For example, to create a new record or document:
	 * {{{
	 * $post = Posts::create(); // Creates a new object, which doesn't exist in the database yet
	 * $post->title = "My post";
	 * $success = $post->save();
	 * }}}
	 *
	 * It is also used to update existing database objects, as in the following:
	 * {{{
	 * $post = Posts::first($id);
	 * $post->title = "Revised title";
	 * $success = $post->save();
	 * }}}
	 *
	 * By default, an object's data will be checked against the validation rules of the model it is
	 * bound to. Any validation errors that result can then be accessed through the `errors()`
	 * method.
	 *
	 * {{{
	 * if (!$post->save($someData)) {
	 * 	return array('errors' => $post->errors());
	 * }
	 * }}}
	 *
	 * To override the validation checks and save anyway, you can pass the `'validate'` option:
	 *
	 * {{{
	 * $post->title = "We Don't Need No Stinkin' Validation";
	 * $post->body = "I know what I'm doing.";
	 * $post->save(null, array('validate' => false));
	 * }}}
	 *
	 * @see lithium\data\Model::$validates
	 * @see lithium\data\Model::validates()
	 * @see lithium\data\Entity::errors()
	 * @param object $entity The record or document object to be saved in the database. This
	 *               parameter is implicit and should not be passed under normal circumstances.
	 *               In the above example, the call to `save()` on the `$post` object is
	 *               transparently proxied through to the `Posts` model class, and `$post` is passed
	 *               in as the `$entity` parameter.
	 * @param array $data Any data that should be assigned to the record before it is saved.
	 * @param array $options Options:
	 *        - `'callbacks'` _boolean_: If `false`, all callbacks will be disabled before
	 *           executing. Defaults to `true`.
	 *        - `'validate'` _mixed_: If `false`, validation will be skipped, and the record will
	 *          be immediately saved. Defaults to `true`. May also be specified as an array, in
	 *          which case it will replace the default validation rules specified in the
	 *         `$validates` property of the model.
	 *        - `'events'` _mixed_: A string or array defining one or more validation _events_.
	 *          Events are different contexts in which data events can occur, and correspond to the
	 *          optional `'on'` key in validation rules. They will be passed to the validates()
	 *          method if `'validate'` is not `false`.
	 *        - `'whitelist'` _array_: An array of fields that are allowed to be saved to this
	 *          record.
	 *
	 * @return boolean Returns `true` on a successful save operation, `false` on failure.
	 * @filter
	 */
	public function save($entity, $data = null, array $options = array()) {
		$self = static::_object();
		$_meta = array('model' => get_called_class()) + $self->_meta;
		$_schema = $self->schema();

		$defaults = array(
			'validate' => true,
			'events' => $entity->exists() ? 'update' : 'create',
			'whitelist' => null,
			'callbacks' => true,
			'locked' => $self->_meta['locked']
		);
		$options += $defaults;
		$params = compact('entity', 'data', 'options');

		$filter = function($self, $params) use ($_meta, $_schema) {
			$entity = $params['entity'];
			$options = $params['options'];

			if ($params['data']) {
				$entity->set($params['data']);
			}
			if ($rules = $options['validate']) {
				$events = $options['events'];
				$validateOpts = is_array($rules) ? compact('rules','events') : compact('events');

				if (!$entity->validates($validateOpts)) {
					return false;
				}
			}
			if (($whitelist = $options['whitelist']) || $options['locked']) {
				$whitelist = $whitelist ?: array_keys($_schema->fields());
			}

			$type = $entity->exists() ? 'update' : 'create';
			$queryOpts = compact('type', 'whitelist', 'entity') + $options + $_meta;
			$query = $self::invokeMethod('_instance', array('query', $queryOpts));
			return $self::connection()->{$type}($query, $options);
		};

		if (!$options['callbacks']) {
			return $filter(get_called_class(), $params);
		}
		return static::_filter(__FUNCTION__, $params, $filter);
	}

	/**
	 * An important part of describing the business logic of a model class is defining the
	 * validation rules. In Lithium models, rules are defined through the `$validates` class
	 * property, and are used by this method before saving to verify the correctness of the data
	 * being sent to the backend data source.
	 *
	 * Note that these are application-level validation rules, and do not
	 * interact with any rules or constraints defined in your data source. If such constraints fail,
	 * an exception will be thrown by the database layer. The `validates()` method only checks
	 * against the rules defined in application code.
	 *
	 * This method uses the `Validator` class to perform data validation. An array representation of
	 * the entity object to be tested is passed to the `check()` method, along with the model's
	 * validation rules. Any rules defined in the `Validator` class can be used to validate fields.
	 * See the `Validator` class to add custom rules, or override built-in rules.
	 *
	 * @see lithium\data\Model::$validates
	 * @see lithium\util\Validator::check()
	 * @see lithium\data\Entity::errors()
	 * @param string $entity Model entity to validate. Typically either a `Record` or `Document`
	 *               object. In the following example:
	 * {{{
	 * $post = Posts::create($data);
	 * $success = $post->validates();
	 * }}}
	 * The `$entity` parameter is equal to the `$post` object instance.
	 * @param array $options Available options:
	 *              - `'rules'` _array_: If specified, this array will _replace_ the default
	 *                validation rules defined in `$validates`.
	 *              - `'events'` _mixed_: A string or array defining one or more validation
	 *                 _events_. Events are different contexts in which data events can occur, and
	 *                correspond to the optional `'on'` key in validation rules. For example, by
	 *                default, `'events'` is set to either `'create'` or `'update'`, depending on
	 *                whether `$entity` already exists. Then, individual rules can specify
	 *                `'on' => 'create'` or `'on' => 'update'` to only be applied at certain times.
	 *                Using this parameter, you can set up custom events in your rules as well, such
	 *                as `'on' => 'login'`. Note that when defining validation rules, the `'on'` key
	 *                can also be an array of multiple events.
	 * @return boolean Returns `true` if all validation rules on all fields succeed, otherwise
	 *         `false`. After validation, the messages for any validation failures are assigned to
	 *         the entity, and accessible through the `errors()` method of the entity object.
	 * @filter
	 */
	public function validates($entity, array $options = array()) {
		$defaults = array(
			'rules' => $this->validates,
			'events' => $entity->exists() ? 'update' : 'create',
			'model' => get_called_class()
		);
		$options += $defaults;
		$self = static::_object();
		$validator = $self->_classes['validator'];
		$params = compact('entity', 'options');

		$filter = function($parent, $params) use (&$self, $validator) {
			$entity = $params['entity'];
			$options = $params['options'];
			$rules = $options['rules'];
			unset($options['rules']);

			if ($errors = $validator::check($entity->data(), $rules, $options)) {
				$entity->errors($errors);
			}
			return empty($errors);
		};
		return static::_filter(__FUNCTION__, $params, $filter);
	}

	/**
	 * Deletes the data associated with the current `Model`.
	 *
	 * @param object $entity Entity to delete.
	 * @param array $options Options.
	 * @return boolean Success.
	 * @filter
	 */
	public function delete($entity, array $options = array()) {
		$params = compact('entity', 'options');

		return static::_filter(__FUNCTION__, $params, function($self, $params) {
			$options = $params + $params['options'] + array('model' => $self, 'type' => 'delete');
			unset($options['options']);

			$query = $self::invokeMethod('_instance', array('query', $options));
			return $self::connection()->delete($query, $options);
		});
	}

	/**
	 * Update multiple records or documents with the given data, restricted by the given set of
	 * criteria (optional).
	 *
	 * @param mixed $data Typically an array of key/value pairs that specify the new data with which
	 *              the records will be updated. For SQL databases, this can optionally be an SQL
	 *              fragment representing the `SET` clause of an `UPDATE` query.
	 * @param mixed $conditions An array of key/value pairs representing the scope of the records
	 *              to be updated.
	 * @param array $options Any database-specific options to use when performing the operation. See
	 *              the `delete()` method of the corresponding backend database for available
	 *              options.
	 * @return boolean Returns `true` if the update operation succeeded, otherwise `false`.
	 * @filter
	 */
	public static function update($data, $conditions = array(), array $options = array()) {
		$params = compact('data', 'conditions', 'options');

		return static::_filter(__FUNCTION__, $params, function($self, $params) {
			$options = $params + $params['options'] + array('model' => $self, 'type' => 'update');
			unset($options['options']);

			$query = $self::invokeMethod('_instance', array('query', $options));
			return $self::connection()->update($query, $options);
		});
	}

	/**
	 * Remove multiple documents or records based on a given set of criteria. **WARNING**: If no
	 * criteria are specified, or if the criteria (`$conditions`) is an empty value (i.e. an empty
	 * array or `null`), all the data in the backend data source (i.e. table or collection) _will_
	 * be deleted.
	 *
	 * @param mixed $conditions An array of key/value pairs representing the scope of the records or
	 *              documents to be deleted.
	 * @param array $options Any database-specific options to use when performing the operation. See
	 *              the `delete()` method of the corresponding backend database for available
	 *              options.
	 * @return boolean Returns `true` if the remove operation succeeded, otherwise `false`.
	 * @filter
	 */
	public static function remove($conditions = array(), array $options = array()) {
		$params = compact('conditions', 'options');

		return static::_filter(__FUNCTION__, $params, function($self, $params) {
			$options = $params['options'] + $params + array('model' => $self, 'type' => 'delete');
			unset($options['options']);

			$query = $self::invokeMethod('_instance', array('query', $options));
			return $self::connection()->delete($query, $options);
		});
	}

	/**
	 * Gets the connection object to which this model is bound. Throws exceptions if a connection
	 * isn't set, or if the connection named isn't configured.
	 *
	 * @return object Returns an instance of `lithium\data\Source` from the connection configuration
	 *         to which this model is bound.
	 */
	public static function &connection() {
		$self = static::_object();
		$connections = $self->_classes['connections'];
		$name = isset($self->_meta['connection']) ? $self->_meta['connection'] : null;

		if ($conn = $connections::get($name)) {
			return $conn;
		}
		throw new ConfigException("The data connection `{$name}` is not configured.");
	}

	/**
	 * Wraps `StaticObject::applyFilter()` to account for object instances.
	 *
	 * @see lithium\core\StaticObject::applyFilter()
	 * @param string $method
	 * @param mixed $closure
	 */
	public static function applyFilter($method, $closure = null) {
		$instance = static::_object();
		$methods = (array) $method;

		foreach ($methods as $method) {
			if (!isset($instance->_instanceFilters[$method])) {
				$instance->_instanceFilters[$method] = array();
			}
			$instance->_instanceFilters[$method][] = $closure;
		}
	}

	/**
	 * Wraps `StaticObject::_filter()` to account for object instances.
	 *
	 * @see lithium\core\StaticObject::_filter()
	 * @param string $method
	 * @param array $params
	 * @param mixed $callback
	 * @param array $filters Defaults to empty array.
	 * @return object
	 */
	protected static function _filter($method, $params, $callback, $filters = array()) {
		if (!strpos($method, '::')) {
			$method = get_called_class() . '::' . $method;
		}
		list($class, $method) = explode('::', $method, 2);
		$instance = static::_object();

		if (isset($instance->_instanceFilters[$method])) {
			$filters = array_merge($instance->_instanceFilters[$method], $filters);
		}
		return parent::_filter($method, $params, $callback, $filters);
	}

	protected static function &_object() {
		$class = get_called_class();

		if (!isset(static::$_instances[$class])) {
			static::$_instances[$class] = new $class();
			static::config();
		}
		$object = static::_initialize($class);
		return $object;
	}

	/**
	 * Iterates through relationship types to construct relation map.
	 *
	 * @return void
	 * @todo See if this can be rewritten to be lazy.
	 */
	protected static function _relationsToLoad() {
		try {
			if (!static::connection()) {
				return;
			}
		} catch (ConfigExcepton $e) {
			return;
		}
		$self = static::_object();

		foreach ($self->_relationTypes as $type) {
			$self->$type = Set::normalize($self->$type);
			foreach ($self->$type as $name => $config) {
				$self->_relationsToLoad[$name] = $type;
			}
		}
	}

	/**
	 * Exports an array of custom finders which use the filter system to wrap around `find()`.
	 *
	 * @return void
	 */
	protected static function _findFilters() {
		$self = static::_object();
		$_query = $self->_query;

		return array(
			'first' => function($self, $params, $chain) {
				$params['options']['limit'] = 1;
				$data = $chain->next($self, $params, $chain);
				$data = is_object($data) ? $data->rewind() : $data;
				return $data ?: null;
			},
			'list' => function($self, $params, $chain) {
				$result = array();
				$meta = $self::meta();
				$name = $meta['key'];

				foreach ($chain->next($self, $params, $chain) as $entity) {
					$key = $entity->{$name};
					$result[is_scalar($key) ? $key : (string) $key] = $entity->title();
				}
				return $result;
			},
			'count' => function($self, $params) use ($_query) {
				$model = $self;
				$type = $params['type'];
				$options = array_diff_key($params['options'], $_query);

				if ($options && !isset($params['options']['conditions'])) {
					$options = array('conditions' => $options);
				} else {
					$options = $params['options'];
				}
				$options += array('type' => 'read') + compact('model');
				$query = $self::invokeMethod('_instance', array('query', $options));
				return $self::connection()->calculation('count', $query, $options);
			}
		);
	}

	/**
	 * Reseting the model
	 */
	public static function reset() {
		$class = get_called_class();
		unset(static::$_instances[$class]);
	}
}

?>