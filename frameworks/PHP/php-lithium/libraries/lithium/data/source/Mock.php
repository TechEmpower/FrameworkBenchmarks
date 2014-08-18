<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data\source;

/**
 * The `Mock` data source is used behind-the-scenes when a model does not use a backend data source.
 * It implements the necessary methods, but does not support querying and has no storage backend.
 * It can create generic entities for use in forms and elsewhere within the framework. This allows
 * developers to create domain objects with business logic and schemas, without worrying about
 * backend storage.
 */
class Mock extends \lithium\data\Source {

	protected $_classes = array(
		'entity' => 'lithium\data\Entity',
		'set' => 'lithium\data\Collection',
		'relationship' => 'lithium\data\model\Relationship',
		'schema' => 'lithium\data\Schema'
	);

	public function connect() {
		return true;
	}

	public function disconnect() {
		return true;
	}

	public function sources($class = null) {
		return array();
	}

	public function describe($entity, $fields = array(), array $meta = array()) {
		return $this->_instance('schema', compact('fields'));
	}

	public function relationship($class, $type, $name, array $options = array()) {
		return false;
	}

	public function create($query, array $options = array()) {
		return false;
	}

	public function read($query, array $options = array()) {
		return false;
	}

	public function update($query, array $options = array()) {
		return false;
	}

	public function delete($query, array $options = array()) {
		return false;
	}
}

?>