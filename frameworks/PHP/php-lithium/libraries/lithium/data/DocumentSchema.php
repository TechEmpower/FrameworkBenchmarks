<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data;

class DocumentSchema extends \lithium\data\Schema {

	protected $_classes = array(
		'entity' => 'lithium\data\entity\Document',
		'set'    => 'lithium\data\collection\DocumentSet'
	);

	protected $_handlers = array();

	protected function _init() {
		$this->_autoConfig[] = 'handlers';
		parent::_init();
	}

	public function cast($object, $key, $data, array $options = array()) {
		$defaults = array(
			'parent' => null,
			'pathKey' => null,
			'model' => null,
			'wrap' => true,
			'first' => false
		);
		$options += $defaults;

		$basePathKey = $options['pathKey'];
		$model = (!$options['model'] && $object) ? $object->model() : $options['model'];
		$classes = $this->_classes;

		$fieldName = is_int($key) ? null : $key;
		$pathKey = $basePathKey;

		if ($fieldName) {
			$pathKey = $basePathKey ? "{$basePathKey}.{$fieldName}" : $fieldName;
		}

		if ($data instanceof $classes['set'] || $data instanceof $classes['entity']) {
			return $data;
		}
		if (is_object($data) && !$this->is('array', $pathKey)) {
			return $data;
		}
		return $this->_castArray($object, $data, $pathKey, $options, $defaults);
	}

	protected function _castArray($object, $val, $pathKey, $options, $defaults) {
		$isArray = $this->is('array', $pathKey) && (!$object instanceof $this->_classes['set']);
		$isObject = ($this->type($pathKey) === 'object');
		$valIsArray = is_array($val);
		$numericArray = false;
		$class = 'entity';

		if (!$valIsArray && !$isArray) {
			return $this->_castType($val, $pathKey);
		}

		if ($valIsArray) {
			$numericArray = !$val || array_keys($val) === range(0, count($val) - 1);
		}

		if ($isArray || ($numericArray && !$isObject)) {
			$val = $valIsArray ? $val : array($val);
			$class = 'set';
		}

		if ($options['wrap']) {
			$config = array(
				'data' => $val,
				'parent' => $options['parent'],
				'model' => $options['model'],
				'schema' => $this
			);
			$config += compact('pathKey') + array_diff_key($options, $defaults);
			$val = $this->_instance($class, $config);
		} elseif ($class === 'set') {
			$val = $val ?: array();
			foreach ($val as &$value) {
				$value = $this->_castType($value, $pathKey);
			}
		}
		return $val;
	}

	/**
	 * Casts a scalar (non-object/array) value to its corresponding database-native value or custom
	 * value object based on a handler assigned to `$field`'s data type.
	 *
	 * @param mixed $value The value to be cast.
	 * @param string $field The name of the field that `$value` is or will be stored in. If it is a
	 *               nested field, `$field` should be the full dot-separated path to the
	 *               sub-object's field.
	 * @return mixed Returns the result of `$value`, modified by a matching handler data type
	 *               handler, if available.
	 */
	protected function _castType($value, $field) {
		if ($this->is('null', $field) && ($value === null || $value === "")) {
			return null;
		}
		if (!is_scalar($value)) {
			return $value;
		}
		$type = $this->type($field);
		return isset($this->_handlers[$type]) ? $this->_handlers[$type]($value) : $value;
	}
}

?>