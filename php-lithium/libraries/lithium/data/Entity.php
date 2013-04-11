<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data;

use BadMethodCallException;
use UnexpectedValueException;
use lithium\data\Collection;
use lithium\analysis\Inspector;

/**
 * `Entity` is a smart data object which represents data such as a row or document in a
 * database. Entities have fields (often known as columns in databases), and track changes to its
 * fields, as well as associated validation errors, etc.
 *
 * The `Entity` class can also be used as a base class for your own custom data objects, and is the
 * basis for generating forms with the `Form` helper.
 *
 * @see lithium\template\helper\Form
 */
class Entity extends \lithium\core\Object {

	/**
	 * Fully-namespaced class name of model that this record is bound to. Instance methods declared
	 * in the model may be called on the entity. See the `Model` class documentation for more
	 * information.
	 *
	 * @see lithium\data\Model
	 * @see lithium\data\Entity::__call()
	 * @var string
	 */
	protected $_model = null;

	/**
	 * Associative array of the entity's fields and values.
	 *
	 * @var array
	 */
	protected $_data = array();

	/**
	 * An array containing all related records and recordsets, keyed by relationship name, as
	 * defined in the bound model class.
	 *
	 * @var array
	 */
	protected $_relationships = array();

	/**
	 * If this record is chained off of another, contains the origin object.
	 *
	 * @var object
	 */
	protected $_parent = null;

	/**
	 * The list of validation errors associated with this object, where keys are field names, and
	 * values are arrays containing one or more validation error messages.
	 *
	 * @see lithium\data\Entity::errors()
	 * @var array
	 */
	protected $_errors = array();

	/**
	 * Contains the values of updated fields. These values will be persisted to the backend data
	 * store when the document is saved.
	 *
	 * @var array
	 */
	protected $_updated = array();

	/**
	 * An array of key/value pairs corresponding to fields that should be updated using atomic
	 * incrementing / decrementing operations. Keys match field names, and values indicate the value
	 * each field should be incremented or decremented by.
	 *
	 * @see lithium\data\Entity::increment()
	 * @see lithium\data\Entity::decrement()
	 * @var array
	 */
	protected $_increment = array();

	/**
	 * A flag indicating whether or not this entity exists. Set to `false` if this is a
	 * newly-created entity, or if this entity has been loaded and subsequently deleted. Set to
	 * `true` if the entity has been loaded from the database, or has been created and subsequently
	 * saved.
	 *
	 * @var boolean
	 */
	protected $_exists = false;

	/**
	 * A local copy of the schema definition. This is the same as `lithium\data\Model::$_schema`,
	 * but can be defined here if this is a one-off object or class used for a single purpose, i.e.
	 * to create a form.
	 *
	 * @var array
	 */
	protected $_schema = array();

	/**
	 * Auto configuration.
	 *
	 * @var array
	 */
	protected $_autoConfig = array(
		'classes' => 'merge', 'parent', 'schema', 'data',
		'model', 'exists', 'pathKey', 'relationships'
	);

	/**
	 * Creates a new record object with default values.
	 *
	 * Options defined:
	 * - 'data' _array_: Data to enter into the record. Defaults to an empty array.
	 * - 'model' _string_: Class name that provides the data-source for this record.
	 *   Defaults to `null`.
	 *
	 * @param array $config
	 * @return object Record object.
	 */
	public function __construct(array $config = array()) {
		$defaults = array('model' => null, 'data' => array(), 'relationships' => array());
		parent::__construct($config + $defaults);
	}

	protected function _init() {
		parent::_init();
		$this->_updated = $this->_data;
	}

	/**
	 * Overloading for reading inaccessible properties.
	 *
	 * @param string $name Property name.
	 * @return mixed Result.
	 */
	public function &__get($name) {
		if (isset($this->_relationships[$name])) {
			return $this->_relationships[$name];
		}
		if (isset($this->_updated[$name])) {
			return $this->_updated[$name];
		}
		$null = null;
		return $null;
	}

	/**
	 * Overloading for writing to inaccessible properties.
	 *
	 * @param string $name Property name.
	 * @param string $value Property value.
	 * @return mixed Result.
	 */
	public function __set($name, $value = null) {
		if (is_array($name) && !$value) {
			return array_map(array(&$this, '__set'), array_keys($name), array_values($name));
		}
		$this->_updated[$name] = $value;
	}

	/**
	 * Overloading for calling `isset()` or `empty()` on inaccessible properties.
	 *
	 * @param string $name Property name.
	 * @return mixed Result.
	 */
	public function __isset($name) {
		return isset($this->_updated[$name]) || isset($this->_relationships[$name]);
	}

	/**
	 * Magic method that allows calling of model methods on this record instance, i.e.:
	 * {{{
	 * $record->validates();
	 * }}}
	 *
	 * @param string $method Method name caught by `__call()`.
	 * @param array $params Arguments given to the above `$method` call.
	 * @return mixed
	 */
	public function __call($method, $params) {
		if (($model = $this->_model) && method_exists($model, '_object')) {
			array_unshift($params, $this);
			$class = $model::invokeMethod('_object');
			return call_user_func_array(array(&$class, $method), $params);
		}
		$message = "No model bound to call `{$method}`.";
		throw new BadMethodCallException($message);
	}

	/**
	 * Custom check to determine if our given magic methods can be responded to.
	 *
	 * @param  string  $method     Method name.
	 * @param  bool    $internal   Interal call or not.
	 * @return bool
	 */
	public function respondsTo($method, $internal = false) {
		$class = $this->_model;
		$modelRespondsTo = false;
		$parentRespondsTo = parent::respondsTo($method, $internal);
		$staticRespondsTo = $class::respondsTo($method, $internal);
		if (method_exists($class, '_object')) {
			$model = $class::invokeMethod('_object');
			$modelRespondsTo = $model->respondsTo($method);
		} else {
			$modelRespondsTo = Inspector::isCallable($class, $method, $internal);
		}
		return $parentRespondsTo || $staticRespondsTo || $modelRespondsTo;
	}

	/**
	 * Allows several properties to be assigned at once, i.e.:
	 * {{{
	 * $record->set(array('title' => 'Lorem Ipsum', 'value' => 42));
	 * }}}
	 *
	 * @param array $data An associative array of fields and values to assign to this `Entity`
	 *        instance.
	 * @return void
	 */
	public function set(array $data) {
		foreach ($data as $name => $value) {
			$this->__set($name, $value);
		}
	}

	/**
	 * Access the data fields of the record. Can also access a $named field.
	 *
	 * @param string $name Optionally included field name.
	 * @return mixed Entire data array if $name is empty, otherwise the value from the named field.
	 */
	public function data($name = null) {
		if ($name) {
			return $this->__get($name);
		}
		return $this->to('array');
	}

	/**
	 * Returns the model which this entity is bound to.
	 *
	 * @return string The fully qualified model class name.
	 */
	public function model() {
		return $this->_model;
	}

	public function schema($field = null) {
		$schema = null;

		switch (true) {
			case (is_object($this->_schema)):
				$schema = $this->_schema;
			break;
			case ($model = $this->_model):
				$schema = $model::schema();
			break;
		}
		if ($schema) {
			return $field ? $schema->fields($field) : $schema;
		}
		return array();
	}

	/**
	 * Access the errors of the record.
	 *
	 * @see lithium\data\Entity::$_errors
	 * @param array|string $field If an array, overwrites `$this->_errors`. If a string, and
	 *        `$value` is not `null`, sets the corresponding key in `$this->_errors` to `$value`.
	 * @param string $value Value to set.
	 * @return mixed Either the `$this->_errors` array, or single value from it.
	 */
	public function errors($field = null, $value = null) {
		if ($field === null) {
			return $this->_errors;
		}
		if (is_array($field)) {
			return ($this->_errors = $field);
		}
		if ($value === null && isset($this->_errors[$field])) {
			return $this->_errors[$field];
		}
		if ($value !== null) {
			return $this->_errors[$field] = $value;
		}
		return $value;
	}

	/**
	 * A flag indicating whether or not this record exists.
	 *
	 * @return boolean `True` if the record was `read` from the data-source, or has been `create`d
	 *         and `save`d. Otherwise `false`.
	 */
	public function exists() {
		return $this->_exists;
	}

	/**
	 * Called after an `Entity` is saved. Updates the object's internal state to reflect the
	 * corresponding database entity, and sets the `Entity` object's key, if this is a newly-created
	 * object. **Do not** call this method if you intend to update the database's copy of the
	 * entity. Instead, see `Model::save()`.
	 *
	 * @see lithium\data\Model::save()
	 * @param mixed $id The ID to assign, where applicable.
	 * @param array $data Any additional generated data assigned to the object by the database.
	 * @param array $options Method options:
	 *              - `'materialize'` _boolean_: Determines whether or not the flag should be set
	 *                that indicates that this entity exists in the data store. Defaults to `true`.
	 *              - `'dematerialize'` _boolean_: If set to `true`, indicates that this entity has
	 *                been deleted from the data store and no longer exists. Defaults to `false`.
	 * @return void
	 */
	public function sync($id = null, array $data = array(), array $options = array()) {
		$defaults = array('materialize' => true, 'dematerialize' => false);
		$options += $defaults;
		$model = $this->_model;
		$key = array();

		if ($options['materialize']) {
			$this->_exists = true;
		}
		if ($options['dematerialize']) {
			$this->_exists = false;
		}
		if ($id && $model) {
			$key = $model::meta('key');
			$key = is_array($key) ? array_combine($key, $id) : array($key => $id);
		}
		$this->_data = $this->_updated = ($key + $data + $this->_updated);
	}

	/**
	 * Safely (atomically) increments the value of the specified field by an arbitrary value.
	 * Defaults to `1` if no value is specified. Throws an exception if the specified field is
	 * non-numeric.
	 *
	 * @param string $field The name of the field to be incremented.
	 * @param string $value The value to increment the field by. Defaults to `1` if this parameter
	 *               is not specified.
	 * @return integer Returns the current value of `$field`, based on the value retrieved from the
	 *         data source when the entity was loaded, plus any increments applied. Note that it may
	 *         not reflect the most current value in the persistent backend data source.
	 * @throws UnexpectedValueException Throws an exception when `$field` is set to a non-numeric
	 *         type.
	 */
	public function increment($field, $value = 1) {
		if (!isset($this->_updated[$field])) {
			return $this->_updated[$field] = $value;
		}
		if (!is_numeric($this->_updated[$field])) {
			throw new UnexpectedValueException("Field '{$field}' cannot be incremented.");
		}
		return $this->_updated[$field] += $value;
	}

	/**
	 * Decrements a field by the specified value. Works identically to `increment()`, but in
	 * reverse.
	 *
	 * @see lithium\data\Entity::increment()
	 * @param string $field The name of the field to decrement.
	 * @param string $value The value by which to decrement the field. Defaults to `1`.
	 * @return integer Returns the new value of `$field`, after modification.
	 */
	public function decrement($field, $value = 1) {
		return $this->increment($field, $value * -1);
	}

	/**
	 * Gets the current state for a given field or, if no field is given, gets the array of
	 * fields modified on this entity.
	 *
	 * @param string The field name to check its state.
	 * @return mixed Returns `true` if a field is given and was updated, `false` otherwise and
	 *		   `null` if the field was not set at all. If no field is given returns an arra
	 *		   where the keys are entity field names, and the values are `true` for changed
	 *         fields.
	 */
	public function modified($field = null) {
		if ($field) {
			if (!isset($this->_updated[$field]) && !isset($this->_data[$field])) {
				return null;
			}

			if (!array_key_exists($field, $this->_updated)) {
				return false;
			}

			$value = $this->_updated[$field];
			if (is_object($value) && method_exists($value, 'modified')) {
				$modified = $value->modified();
				return $modified === true || is_array($modified) && in_array(true, $modified, true);
			}

			$isSet = isset($this->_data[$field]);
			return !$isSet || ($this->_data[$field] !== $this->_updated[$field]);
		}

		$fields = array_fill_keys(array_keys($this->_data), false);

		foreach ($this->_updated as $field => $value) {
			if (is_object($value) && method_exists($value, 'modified')) {
				if (!isset($this->_data[$field])) {
					$fields[$field] = true;
					continue;
				}
				$modified = $value->modified();

				$fields[$field] = (
					$modified === true || is_array($modified) && in_array(true, $modified, true)
				);
			} else {
				$fields[$field] = (
					!isset($fields[$field]) || $this->_data[$field] !== $this->_updated[$field]
				);
			}
		}
		return $fields;
	}

	public function export(array $options = array()) {
		return array(
			'exists'    => $this->_exists,
			'data'      => $this->_data,
			'update'    => $this->_updated,
			'increment' => $this->_increment
		);
	}

	/**
	 * Configures protected properties of an `Entity` so that it is parented to `$parent`.
	 *
	 * @param object $parent
	 * @param array $config
	 * @return void
	 */
	public function assignTo($parent, array $config = array()) {
		foreach ($config as $key => $val) {
			$this->{'_' . $key} = $val;
		}
		$this->_parent =& $parent;
	}

	/**
	 * Converts the data in the record set to a different format, i.e. an array.
	 *
	 * @param string $format currently only `array`
	 * @param array $options
	 * @return mixed
	 */
	public function to($format, array $options = array()) {
		switch ($format) {
			case 'array':
				$data = $this->_updated;
				$rel = array_map(function($obj) { return $obj->data(); }, $this->_relationships);
				$data = $rel + $data;
				$result = Collection::toArray($data, $options);
			break;
			case 'string':
				$result = $this->__toString();
			break;
			default:
				$result = $this;
			break;
		}
		return $result;
	}

	/**
	 * Returns a string representation of the `Entity` instance, based on the result of the
	 * `'title'` meta value of the bound model class.
	 *
	 * @return string Returns the generated title of the object.
	 */
	public function __toString() {
		return (string) $this->__call('title', array());
	}
}

?>