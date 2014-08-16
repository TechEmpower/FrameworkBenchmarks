<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data\model;

use lithium\util\Set;
use lithium\data\Source;
use lithium\core\ConfigException;
use InvalidArgumentException;

/**
 * The `Query` class acts as a container for all information necessary to perform a particular
 * database operation. Each `Query` object instance has a type, which is usually one of `'create'`,
 * `'read'`, `'update'` or `'delete'`.
 *
 * Because of this, `Query` objects are the primary method of communication between `Model` classes
 * and backend data sources. This helps to keep APIs abstract and flexible, since a model is only
 * required to call a single method against its backend. Since the `Query` object simply acts as a
 * structured data container, each backend can choose how to operate on the data the `Query`
 * contains. See each class method for more details on what data this class supports.
 *
 * @see lithium\data\Model
 * @see lithium\data\Source
 */
class Query extends \lithium\core\Object {

	/**
	 * The 'type' of query to be performed. This is either `'create'`, `'read'`, `'update'` or
	 * `'delete'`, and corresponds to the method to be executed.
	 *
	 * @var string
	 */
	protected $_type = null;

	/**
	 * Array containing mappings of relationship and field names, which allow database results to
	 * be mapped to the correct objects.
	 *
	 * @var array
	 */
	protected $_map = array();

	/**
	 * If a `Query` is bound to a `Record` or `Document` object (i.e. for a `'create'` or
	 * `'update'` query).
	 *
	 * @var object
	 */
	protected $_entity = null;

	/**
	 * An array of data used in a write context. Only used if no binding object is present in the
	 * `$_entity` property.
	 *
	 * @var array
	 */
	protected $_data = array();

	/**
	 * A query can be assigned its own custom schema object, using the `schema()` method. If this
	 * is not assigned, then the model associated with the query will be used to get the schema
	 * information.
	 *
	 * @var object
	 */
	protected $_schema = null;

	/**
	 * Classes used by `Query`.
	 *
	 * @var array
	 */
	protected $_classes = array(
		'schema' => 'lithium\data\Schema'
	);

	/**
	 * The query's fields
	 *
	 * @see lithium\data\model\Query::fields()
	 *
	 * @var array
	 */
	protected $_fields = array(0 => array(), 1 => array());

	/**
	 * Count the number of identical models in a query for building
	 * unique aliases
	 *
	 * @see lithium\data\model\Query::alias()
	 *
	 * @var array
	 */
	protected $_alias = array();

	/**
	 * Map beetween generated aliases and corresponding relation paths
	 *
	 * @see lithium\data\model\Query::alias()
	 *
	 * @var array
	 */
	protected $_paths = array();

	/**
	 * Map beetween generated aliases and corresponding models.
	 *
	 * @see lithium\data\model\Query::alias()
	 *
	 * @var array
	 */
	protected $_models = array();

	/**
	 * Auto configuration properties.
	 *
	 * @var array
	 */
	protected $_autoConfig = array('type', 'map');

	/**
	 * Initialization methods on construct
	 *
	 * @var array
	 */

	protected $_initializers = array(
		'model', 'entity', 'conditions', 'having', 'group', 'order',
		'limit', 'offset', 'page', 'data', 'calculate', 'schema', 'comment'
	);

	/**
	 * Boolean indicate if the query is built or not
	 *
	 * @var string
	 */
	protected $_built = false;

	/**
	 * Class constructor, which initializes the default values this object supports.
	 * Even though only a specific list of configuration parameters is available
	 * by default, the `Query` object uses the `__call()` method to implement
	 * automatic getters and setters for any arbitrary piece of data.
	 *
	 * This means that any information may be passed into the constructor may be
	 * used by the backend data source executing the query (or ignored, if support
	 * is not implemented). This is useful if, for example, you wish to extend a
	 * core data source and implement custom fucntionality.
	 *
	 * @param array $config Config options:
	 *        - `'type'` _string_: The type of the query (`read`, `insert`, `update`, `delete`).
	 *        - `'entity'` _object_: The base entity to query on. If set `'model'` is optionnal.
	 *        - `'model'` _string_: The base model to query on.
	 *        - `'source'` _string_: The name of the table/collection. Unnecessary
	 *          if `model` is set.
	 *        - `'alias'` _string_: Alias for the source. Unnecessary if `model` is set.
	 *        - `'schema'` _object_: A schema model. Unnecessary if `model` is set.
	 *        - `'fields'` _array_: The fields to retreive.
	 *        - `'conditions'` _array_: The conditions of the queries
	 *        - `'having'` _array_: The having conditions of the queries
	 *        - `'group'` _string_: The group by parameter.
	 *        - `'order'` _string_: The order by parameter.
	 *        - `'limit'` _string_: The limit parameter.
	 *        - `'offset'` _string_: The offset of the `limit` options.
	 *        - `'page'` _string_: Convenience parameter for setting the `offset`:
	 *          `offset` = `page` * `limit`.
	 *        - `'with'` _array_: Contain dependencies. Works only if `model` is set.
	 *        - `'joins'` _array_: Contain manual join dependencies.
	 *        - `'data'` _array_: Datas for update queries.
	 *        - `'whitelist'` _array_: Allowed fields for updating queries.
	 *        - `'calculate'` _string_: Alias name of the count.
	 *        - `'comment'` _string_: Comment for the query.
	 *        - `'map'` _object_: Unnecessary if `model` is set.
	 *        - `'relationships'` _array_: Unnecessary if `model` is set.
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'model' => null,
			'entity' => null,
			'source' => null,
			'alias' => null,
			'fields' => array(),
			'conditions' => array(),
			'having' => array(),
			'group' => null,
			'order' => null,
			'limit' => null,
			'offset' => null,
			'page' => null,
			'with' => array(),
			'joins' => array(),
			'data' => array(),
			'whitelist' => array(),
			'calculate' => null,
			'schema' => null,
			'comment' => null,
			'map' => array(),
			'relationships' => array()
		);
		parent::__construct($config + $defaults);
	}

	protected function _init() {
		parent::_init();
		unset($this->_config['type']);

		$keys = array_keys($this->_config);
		foreach ($this->_initializers as $key) {
			$val = $this->_config[$key];
			if ($val !== null) {
				$this->_config[$key] = is_array($val) ? array() : null;
				$this->{$key}($val);
			}
		}
		if ($list = $this->_config['whitelist']) {
			$this->_config['whitelist'] = array_combine($list, $list);
		}

		if ($this->_entity && !$this->_config['model']) {
			$this->model($this->_entity->model());
		}

		if ($this->_config['with']) {
			if (!$model = $this->model()) {
				throw new ConfigException("The `'with'` option needs a valid binded model.");
			}
			$this->_config['with'] = Set::normalize($this->_config['with']);
		}

		if ($model = $this->model()) {
			$this->alias($this->_config['alias'] ?: $model::meta('name'));
		}

		$this->fields($this->_config['fields']);

		unset($this->_config['entity'], $this->_config['init']);
	}

	/**
	 * Get method of type, i.e. 'read', 'update', 'create', 'delete'.
	 *
	 * @return string
	 */
	public function type() {
		return $this->_type;
	}

	/**
	 * Generates a schema map of the query's result set, where the keys are aliases, and the values
	 * are arrays of field names.
	 *
	 * @param array $map
	 * @return array
	 */
	public function map($map = null) {
		if ($map !== null) {
			$this->_map = $map;
			return $this;
		}
		return $this->_map;
	}

	/**
	 * Accessor method for `Query` calculate values.
	 *
	 * @param string $calculate Value for calculate config setting.
	 * @return mixed Current calculate config value.
	 */
	public function calculate($calculate = null) {
		if ($calculate) {
			$this->_config['calculate'] = $calculate;
			return $this;
		}
		return $this->_config['calculate'];
	}

	/**
	 * Set and get method for the model associated with the `Query`.
	 * Will also set the source table, i.e. `$this->_config['source']`.
	 *
	 * @param string $model
	 * @return string
	 */
	public function model($model = null) {
		if (!$model) {
			return $this->_config['model'];
		}
		$this->_config['model'] = $model;
		$this->_config['source'] = $this->_config['source'] ?: $model::meta('source');
		return $this;
	}

	/**
	 * Set and get method for conditions.
	 *
	 * If no conditions are set in query, it will ask the bound entity for condition array.
	 *
	 * @param mixed $conditions String or array to append to existing conditions.
	 * @return array Returns an array of all conditions applied to this query.
	 */
	public function conditions($conditions = null) {
		if (!$conditions) {
			return $this->_config['conditions'] ?: $this->_entityConditions();
		}
		$conditions = (array) $conditions;
		$this->_config['conditions'] = (array) $this->_config['conditions'];
		$this->_config['conditions'] = array_merge($this->_config['conditions'], $conditions);
		return $this;
	}

	/**
	 * Set and get method for havings.
	 *
	 * @param mixed $having String or array to append to existing having.
	 * @return array Returns an array of all having applied to this query.
	 */
	public function having($having = null) {
		if (!$having) {
			return $this->_config['having'];
		}
		$having = (array) $having;
		$this->_config['having'] = (array) $this->_config['having'];
		$this->_config['having'] = array_merge($this->_config['having'], $having);
		return $this;
	}

	/**
	 * Set, get or reset fields option for query.
	 *
	 * Usage:
	 * {{{
	 *	// to add a field
	 *   $query->fields('created');
	 * }}}
	 * {{{
	 *	// to add several fields
	 *   $query->fields(array('title','body','modified'));
	 * }}}
	 * {{{
	 *	// to reset fields to none
	 *   $query->fields(false);
	 *   // should be followed by a 2nd call to fields with required fields
	 * }}}
	 *
	 * @param mixed $fields string, array or `false`
	 * @param boolean $overwrite If `true`, existing fields will be removed before adding `$fields`.
	 * @return array Returns an array containing all fields added to the query.
	 */
	public function fields($fields = null, $overwrite = false) {
		if ($fields === false || $overwrite) {
			$this->_fields = array(0 => array(), 1 => array());
		}
		if ($fields === null) {
			return array_merge(array_keys($this->_fields[1]), $this->_fields[0]);
		}
		if (!$fields) {
			return $this;
		}
		$fields = is_array($fields) ? $fields : array($fields);
		foreach ($fields as $key => $field) {
			if (is_string($field)) {
				$this->_fields[1][$field] = true;
			} elseif (is_array($field) && !is_numeric($key)) {
				foreach ($field as &$val) {
					$val = $key . '.' . $val;
				}
				$this->fields($field);
			} else {
				$this->_fields[0][] = $field;
			}
		}
		return $this;
	}

	/**
	 * Set and get method for query's limit of amount of records to return
	 *
	 * @param integer $limit
	 * @return integer
	 */
	public function limit($limit = null) {
		if ($limit) {
			$this->_config['limit'] = intval($limit);
			return $this;
		}
		if ($limit === false) {
			$this->_config['limit'] = null;
			return $this;
		}
		return $this->_config['limit'];
	}

	/**
	 * Set and get method for query's offset, i.e. which records to get
	 *
	 * @param integer $offset
	 * @return integer
	 */
	public function offset($offset = null) {
		if ($offset !== null) {
			$this->_config['offset'] = intval($offset);
			return $this;
		}
		return $this->_config['offset'];
	}

	/**
	 * Set and get method for page, in relation to limit, of which records to get
	 *
	 * @param integer $page
	 * @return integer
	 */
	public function page($page = null) {
		if ($page) {
			$this->_config['page'] = $page = (intval($page) ?: 1);
			$this->offset(($page - 1) * $this->_config['limit']);
			return $this;
		}
		return $this->_config['page'];
	}

	/**
	 * Set and get method for the query's order specification.
	 *
	 * @param array|string $order
	 * @return mixed
	 */
	public function order($order = null) {
		if ($order) {
			$this->_config['order'] = $order;
			return $this;
		}
		return $this->_config['order'];
	}

	/**
	 * Set and get method for the `Query` group config setting.
	 *
	 * @param string $group New group config setting.
	 * @return mixed Current group config setting.
	 */
	public function group($group = null) {
		if ($group) {
			$this->_config['group'] = $group;
			return $this;
		}
		if ($group === false) {
			$this->_config['group'] = null;
			return $this;
		}
		return $this->_config['group'];
	}

	/**
	 * Set and get method for current query's comment.
	 *
	 * Comment will have no effect on query, but will be passed along so data source can log it.
	 *
	 * @param string $comment
	 * @return string
	 */
	public function comment($comment = null) {
		if ($comment) {
			$this->_config['comment'] = $comment;
			return $this;
		}
		return $this->_config['comment'];
	}

	/**
	 * Set and get method for the query's entity instance.
	 *
	 * @param object $entity Reference to the query's current entity object.
	 * @return object Reference to the query's current entity object.
	 */
	public function &entity(&$entity = null) {
		if ($entity) {
			$this->_entity = $entity;
			return $this;
		}
		return $this->_entity;
	}

	/**
	 * Set and get method for the query's record's data.
	 *
	 * @param array $data if set, will set given array.
	 * @return array Empty array if no data, array of data if the record has it.
	 */
	public function data($data = array()) {
		$bind =& $this->_entity;

		if ($data) {
			$bind ? $bind->set($data) : $this->_data = array_merge($this->_data, $data);
			return $this;
		}
		$data = $bind ? $bind->data() : $this->_data;
		return ($list = $this->_config['whitelist']) ? array_intersect_key($data, $list) : $data;
	}

	/**
	 * Set and get the relationships.
	 *
	 * @param string $relpath A dotted path.
	 * @param array $config the config array to set.
	 * @return mixed The relationships array or a relationship array if `$relpath` is set. Returns
	 *         `null` if a join doesn't exist.
	 * @throws InvalidArgumentException
	 */
	public function relationships($relpath = null, $config = null) {
		if ($config) {
			if (!$relpath) {
				throw new InvalidArgumentException("The relation dotted path is empty.");
			}
			if (isset($config['model']) && isset($config['alias'])) {
				$this->_models[$config['alias']] = $config['model'];
			}
			$this->_config['relationships'][$relpath] = $config;
			return $this;
		}
		if (!$relpath) {
			return $this->_config['relationships'];
		}
		if (isset($this->_config['relationships'][$relpath])) {
			return $this->_config['relationships'][$relpath];
		}
	}

	/**
	 * Set and get the joins
	 *
	 * @param string $name Optional name of join. Unless two parameters are passed, this parameter
	 *               is regonized as `$join`.
	 * @param object|string $join A single query object or an array of query objects
	 * @return mixed The joins array or a join array if `$name` is set. Returns `null` if a join
	 *         doesn't exist.
	 */
	public function joins($name = null, $join = null) {
		if (is_array($name)) {
			$join = $name;
			$name = null;
		}
		if ($join) {
			if (!$name) {
				$this->_config['joins'][] = $join;
			} else {
				$this->_config['joins'][$name] = $join;
			}
			return $this;
		}
		if (!$name) {
			return $this->_config['joins'];
		}
		if (isset($this->_config['joins'][$name])) {
			return $this->_config['joins'][$name];
		}
	}

	/**
	 * Convert the query's properties to the data sources' syntax and return it as an array.
	 *
	 * @param object $source Instance of the data source (`lithium\data\Source`) to use for
	 *        conversion.
	 * @param array $options Options to use when exporting the data.
	 * @return array Returns an array containing a data source-specific representation of a query.
	 */
	public function export(Source $source, array $options = array()) {
		$defaults = array('keys' => array());
		$options += $defaults;

		if ($options['keys']) {
			$keys = array_flip($options['keys']);
		} else {
			$keys =& $this->_config;
		}

		$results = array('type' => $this->_type);

		$apply = array_intersect_key($keys, array_flip($source->methods()));
		$copy = array_diff_key($keys, $apply);

		if (isset($keys['with'])) {
			$this->applyStrategy($source);
		}

		foreach ($apply as $item => $value) {
			$results[$item] = $source->{$item}($this->{$item}(), $this);
		}

		foreach ($copy as $item => $value) {
			$results[$item] = $this->_config[$item];
		}

		if (array_key_exists('data', $keys)) {
			$results['data'] = $this->_exportData();
		}

		if (array_key_exists('source', $keys)) {
			$results['source'] = $source->name($results['source']);
		}

		if (!isset($results['fields'])) {
			return $results;
		}

		$created = array('fields', 'values');

		if (is_array($results['fields']) && array_keys($results['fields']) == $created) {
			$results = $results['fields'] + $results;
		}
		return $results;
	}

	/**
	 * Helper method used by `export()` which delegate the query generation to the datasource.
	 *
	 * @param object $source Instance of the data source (`lithium\data\Source`) to use for
	 *        conversion.
	 */
	public function applyStrategy(Source $source) {
		if ($this->_built) {
			return;
		}
		$this->_built = true;
		if (!$this->_config['with']) {
			return;
		}
		$options = array();
		if (isset($this->_config['strategy'])) {
			$options['strategy'] = $this->_config['strategy'];
		}
		$source->applyStrategy($options, $this);
	}

	/**
	 * Helper method used by `export()` to extract the data either from a bound entity, or from
	 * passed configuration, and filter it through a configured whitelist, if present.
	 *
	 * @return array
	 */
	protected function _exportData() {
		$data = $this->_entity ? $this->_entity->export() : $this->_data;
		if (!$list = $this->_config['whitelist']) {
			return $data;
		}
		$list = array_combine($list, $list);

		if (!$this->_entity) {
			return array_intersect_key($data, $list);
		}

		foreach ($data as $type => $values) {
			if (!is_array($values)) {
				continue;
			}
			$data[$type] = array_intersect_key($values, $list);
		}
		return $data;
	}

	public function schema($field = null) {
		if (is_object($field)) {
			$this->_schema = $field;
			return;
		}
		if ($schema = $this->_schema) {
			return $field ? $schema[$field] : $schema;
		}
		if ($model = $this->model()) {
			return $model::schema($field);
		} else {
			return $this->_instance('schema');
		}
	}

	/**
	 * Get or Set a unique alias for the query or a query's relation if `$relpath` is set.
	 *
	 * @param mixed $alias The value of the alias to set for the passed `$relpath`. For getting an
	 *        alias value set alias to `true`.
	 * @param string $relpath A dotted relation name or `null` for identifying the query's model.
	 * @return string An alias value or `null` for an unexisting `$relpath` alias.
	 */
	public function alias($alias = true, $relpath = null) {
		if ($alias === true) {
			if (!$relpath) {
				return $this->_config['alias'];
			}
			$return = array_search($relpath, $this->_paths);
			return $return ?: null;
		}

		if ($relpath === null) {
			$this->_config['alias'] = $alias;
		}

		if ($relpath === null && ($model = $this->_config['model'])) {
			$this->_models[$alias] = $model;
		}

		$relpath = (string) $relpath;
		unset($this->_paths[array_search($relpath, $this->_paths)]);

		if (!$alias && $relpath) {
			$last = strrpos($relpath, '.');
			$alias = $last ? substr($relpath, $last + 1) : $relpath;
		}

		if (isset($this->_alias[$alias])) {
			$this->_alias[$alias]++;
			$alias .= '__' . $this->_alias[$alias];
		} else {
			$this->_alias[$alias] = 1;
		}

		$this->_paths[$alias] = $relpath;
		return $alias;
	}

	/**
	 * Return the generated aliases mapped to their relation path
	 *
	 * @param object $source Instance of the data source (`lithium\data\Source`) to use for
	 *        conversion.
	 * @return array Map between aliases and their corresponding dotted relation paths.
	 */
	public function paths(Source $source = null) {
		if ($source) {
			$this->applyStrategy($source);
		}
		return $this->_paths;
	}

	/**
	 * Return the generated aliases mapped to their corresponding model
	 *
	 * @param object $source Instance of the data source (`lithium\data\Source`) to use for
	 *        conversion.
	 * @return array Map between aliases and their corresponding fully-namespaced model names.
	 */
	public function models(Source $source = null) {
		if ($source) {
			$this->applyStrategy($source);
		}
		return $this->_models;
	}

	/**
	 * Gets or sets a custom query field which does not have an accessor method.
	 *
	 * @param string $method Query part.
	 * @param array $params Query parameters.
	 * @return mixed Returns the value as set in the `Query` object's constructor.
	 */
	public function __call($method, array $params = array()) {
		if ($params) {
			$this->_config[$method] = current($params);
			return $this;
		}
		return isset($this->_config[$method]) ? $this->_config[$method] : null;
	}

	/**
	 * Custom check to determine if our given magic methods can be responded to.
	 *
	 * @param  string  $method     Method name.
	 * @param  bool    $internal   Interal call or not.
	 * @return bool
	 */
	public function respondsTo($method, $internal = false) {
		return isset($this->_config[$method]) || parent::respondsTo($method, $internal);
	}

	/**
	 * Will return a find first condition on the associated model if a record is connected.
	 * Called by conditions when it is called as a get and no condition is set.
	 *
	 * @return array Returns an array in the following format:
	 *         `([model's primary key'] => [that key set in the record])`.
	 */
	protected function _entityConditions() {
		if (!$this->_entity || !($model = $this->_config['model'])) {
			return;
		}
		$key = $model::key($this->_entity->data());

		if (!$key && $this->_type != "create") {
			throw new ConfigException('No matching primary key found.');
		}
		if (is_array($key)) {
			return $key;
		}

		$key = $model::meta('key');
		$val = $this->_entity->{$key};
		return $val ? array($key => $val) : array();
	}

	/**
	 * Get/set sub queries for the query.
	 * The getter must be called after an export since the sub queries are built
	 * during the export according the export's `mode` option and the query `with` option.
	 *
	 * @see lithium\data\model\Query::export()
	 *
	 * @param string $relpath a dotted relation path
	 * @param string $query a query instance
	 * @return mixed
	 */

	public function childs($relpath = null, $query = null) {
		if (!$model = $this->model()) {
			throw new ConfigException("No binded model.");
		}
		if ($query) {
			$this->_childs[$relpath] = $query;
			return $this;
		}
		return $this->_childs;
	}
}

?>