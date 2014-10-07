<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data;

use lithium\util\Inflector;

class MockSource extends \lithium\data\Source {

	protected $_classes = array(
		'entity' => 'lithium\data\entity\Record',
		'set' => 'lithium\data\collection\RecordSet',
		'relationship' => 'lithium\data\model\Relationship',
		'schema' => 'lithium\data\Schema'
	);

	protected $_mockPosts = array(
		'id' => array('type' => 'int', 'length' => '10', 'null' => false, 'default' => null),
		'user_id' => array(
			'type' => 'int', 'length' => '10', 'null' => true, 'default' => null
		),
		'title' => array(
			'type' => 'varchar', 'length' => '255', 'null' => true, 'default' => null
		),
		'body' => array(
			'type' => 'text', 'length' => null, 'null' => true, 'default' => null
		),
		'created' => array(
			'type' => 'datetime', 'length' => null, 'null' => true, 'default' => null
		),
		'modified' => array(
			'type' => 'datetime', 'length' => null, 'null' => true, 'default' => null
		),
		'status' => array(
			'type' => 'tinyint', 'length' => '1', 'null' => false, 'default' => '0'
		)
	);

	protected $_mockComments = array(
		'id' => array(
			'type' => 'int', 'length' => '10', 'null' => false, 'default' => null
		),
		'comment_type_id' => array(
			'type' => 'int', 'length' => '10', 'null' => false, 'default' => null
		),
		'article_id' => array(
			'type' => 'int', 'length' => '10', 'null' => false, 'default' => null
		),
		'comment_id' => array(
			'type' => 'int', 'length' => '10', 'null' => false, 'default' => null
		),
		'user_id' => array(
			'type' => 'int', 'length' => '10', 'null' => false, 'default' => null
		),
		'created' => array(
			'type' => 'datetime', 'length' => null, 'null' => false, 'default' => null
		),
		'body' => array(
			'type' => 'text', 'length' => null, 'null' => false, 'default' => null
		),
		'subscribed' => array(
			'type' => 'tinyint', 'length' => '1', 'null' => false, 'default' => null
		),
		'published' => array(
			'type' => 'tinyint', 'length' => '1', 'null' => false, 'default' => null
		)
	);

	protected $_mockTags = array(
		'id' => array(
			'type' => 'int', 'length' => '10', 'null' => false, 'default' => null
		),
		'linked' => array(
			'type' => 'int', 'length' => '10', 'null' => true, 'default' => null
		),
		'name' => array(
			'type' => 'varchar', 'length' => '20', 'null' => true, 'default' => null
		),
		'keyname' => array(
			'type' => 'varchar', 'length' => '20', 'null' => true, 'default' => null
		)
	);

	protected $_postsTags = array(
		'id' => array('type' => 'int'),
		'post_id' => array('type' => 'int'),
		'tag_id' => array('type' => 'int'),
	);

	protected $_mockCreators = array(
		'id' => array('type' => 'int'),
		'name' => array(
			'default' => 'Moe',
			'type' => 'string',
			'null' => false
		),
		'sign' => array(
			'default' => 'bar',
			'type' => 'string',
			'null' => false
		),
		'age' => array(
			'default' => 0,
			'type' => 'number',
			'null' => false
		)
	);

	public function connect() {
		return ($this->_isConnected = true);
	}

	public function disconnect() {
		return !($this->_isConnected = false);
	}

	public function sources($class = null) {
		return array('mock_posts', 'mock_comments', 'mock_tags', 'posts_tags');
	}

	public function describe($entity, $schema = array(), array $meta = array()) {
		$source = '_' . Inflector::camelize($entity, false);
		$fields = isset($this->$source) ? $this->$source : array();
		return $this->_instance('schema', compact('fields'));
	}

	public function create($query, array $options = array()) {
		return compact('query', 'options');
	}

	public function read($query, array $options = array()) {
		return compact('query', 'options');
	}

	public function update($query, array $options = array()) {
		return compact('query', 'options');
	}

	public function delete($query, array $options = array()) {
		return compact('query', 'options');
	}

	public function schema($query, $resource = null, $context = null) {

	}

	public function result($type, $resource, $context) {

	}

	public function cast($entity, array $data = array(), array $options = array()) {
		$defaults = array('first' => false);
		$options += $defaults;
		return $options['first'] ? reset($data) : $data;
	}

	public function relationship($class, $type, $name, array $config = array()) {
		$field = Inflector::underscore(Inflector::singularize($name));
		$key = "{$field}_id";
		$primary = $class::meta('key');

		if (is_array($primary)) {
			$key = array_combine($primary, $primary);
		} elseif ($type === 'hasMany' || $type === 'hasOne') {
			if ($type === 'hasMany') {
				$field = Inflector::pluralize($field);
			}
			$secondary = Inflector::underscore(Inflector::singularize($class::meta('name')));
			$key = array($primary => "{$secondary}_id");
		}

		$from = $class;
		$fieldName = $field;
		$config += compact('type', 'name', 'key', 'from', 'fieldName');
		return $this->_instance('relationship', $config);
	}

	public function calculation($type, $query, array $options = array()) {
		$query->calculate($type);
		return compact('query', 'options');
	}
}

?>