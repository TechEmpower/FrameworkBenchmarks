<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data\model;

use lithium\core\Libraries;
use lithium\util\Inflector;
use lithium\core\ConfigException;
use lithium\core\ClassNotFoundException;

/**
 * The `Relationship` class encapsulates the data and functionality necessary to link two model
 * classes together.
 */
class Relationship extends \lithium\core\Object {

	/**
	 * A relationship linking type defined by one document or record (or multiple) being embedded
	 * within another.
	 */
	const LINK_EMBEDDED = 'embedded';

	/**
	 * The reciprocal of `LINK_EMBEDDED`, this defines a linking type wherein an embedded document
	 * references the document that contains it.
	 */
	const LINK_CONTAINED = 'contained';

	/**
	 * A one-to-one or many-to-one relationship in which a key contains an ID value linking to
	 * another document or record.
	 */
	const LINK_KEY = 'key';

	/**
	 * A many-to-many relationship in which a key contains an embedded array of IDs linking to other
	 * records or documents.
	 */
	const LINK_KEY_LIST = 'keylist';

	/**
	 * A relationship defined by a database-native reference mechanism, linking a key to an
	 * arbitrary record or document in another data collection or entirely separate database.
	 */
	const LINK_REF = 'ref';

	/**
	 * Constructs an object that represents a relationship between two model classes.
	 *
	 * @param array $config The relationship's configuration, which defines how the two models in
	 *        question are bound. The available options are:
	 *
	 *        - `'name'` _string_: The name of the relationship in the context of the
	 *          originating model. For example, a `Posts` model might define a relationship to
	 *          a `Users` model like so:
	 * {{{ public $hasMany = array('Author' => array('to' => 'Users')); }}}
	 * In this case, the relationship is bound to the `Users` model, but `'Author'` would be the
	 * relationship name. This is the name with which the relationship is referenced in the
	 * originating model.
	 *        - `'key'` _mixed_: An array of fields that define the relationship, where the
	 *          keys are fields in the originating model, and the values are fields in the
	 *          target model. If the relationship is not deined by keys, this array should be
	 *          empty.
	 *        - `'type'` _string_: The type of relationship. Should be one of `'belongsTo'`,
	 *          `'hasOne'` or `'hasMany'`.
	 *        - `'from'` _string_: The fully namespaced class name of the model where this
	 *          relationship originates.
	 *        - `'to'` _string_: The fully namespaced class name of the model that this
	 *          relationship targets.
	 *        - `'link'` _string_: A constant specifying how the object bound to the
	 *          originating model is linked to the object bound to the target model. For
	 *          relational databases, the only valid value is `LINK_KEY`, which means a foreign
	 *          key in one object matches another key (usually the primary key) in the other.
	 *          For document-oriented and other non-relational databases, different types of
	 *          linking, including key lists, database reference objects (such as MongoDB's
	 *          `MongoDBRef`), or even embedding.
	 *        - `'fields'` _mixed_: An array of the subset of fields that should be selected
	 *          from the related object(s) by default. If set to `true` (the default), all
	 *          fields are selected.
	 *        - `'fieldName'` _string_: The name of the field used when accessing the related
	 *          data in a result set. For example, in the case of `Posts hasMany Comments`, the
	 *          field name defaults to `'comments'`, so comment data is accessed (assuming
	 *          `$post = Posts::first()`) as `$post->comments`.
	 *        - `'constraints'` _mixed_: A string or array containing additional constraints
	 *          on the relationship query. If a string, can contain a literal SQL fragment or
	 *          other database-native value. If an array, maps fields from the related object
	 *          either to fields elsewhere, or to arbitrary expressions. In either case, _the
	 *          values specified here will be literally interpreted by the database_.
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'name' => null,
			'key' => array(),
			'type' => null,
			'to'   => null,
			'from' => null,
			'link' => static::LINK_KEY,
			'fields' => true,
			'fieldName' => null,
			'constraints' => array()
		);
		parent::__construct($config + $defaults);
	}

	protected function _init() {
		parent::_init();
		$config =& $this->_config;
		$type = $config['type'];

		$name = ($type === 'hasOne') ? Inflector::pluralize($config['name']) : $config['name'];
		$config['fieldName'] = $config['fieldName'] ?: lcfirst($name);

		if (!$config['to']) {
			$assoc = preg_replace("/\\w+$/", "", $config['from']) . $name;
			$config['to'] = Libraries::locate('models', $assoc);
		}
		if (!$config['key'] || !is_array($config['key'])) {
			$config['key'] = $this->_keys($config['key']);
		}
	}

	public function data($key = null) {
		if (!$key) {
			return $this->_config;
		}
		return isset($this->_config[$key]) ? $this->_config[$key] : null;
	}

	public function __call($name, $args = array()) {
		return $this->data($name);
	}

	/**
	 * Custom check to determine if our given magic methods can be responded to.
	 *
	 * @param  string  $method     Method name.
	 * @param  bool    $internal   Interal call or not.
	 * @return bool
	 */
	public function respondsTo($method, $internal = false) {
		return is_callable(array($this, $method), true);
	}

	protected function _keys($keys) {
		$config = $this->_config;
		$hasRel = ($related = ($config['type'] === 'belongsTo') ? $config['to'] : $config['from']);

		if (!$hasRel || !$keys) {
			return array();
		}
		if (!class_exists($related)) {
			throw new ClassNotFoundException("Related model class '{$related}' not found.");
		}
		if (!$related::key()) {
			throw new ConfigException("No key defined for related model `{$related}`.");
		}
		$keys = (array) $keys;
		$related = (array) $related::key();

		if (count($keys) !== count($related)) {
			$msg  = "Unmatched keys in relationship `{$config['name']}` between models ";
			$msg .= "`{$config['from']}` and `{$config['to']}`.";
			throw new ConfigException($msg);
		}
		return array_combine($keys, $related);
	}
}

?>