<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\model;

class MockDocumentSource extends \lithium\data\Source {

	protected $_classes = array(
		'entity' => 'lithium\data\entity\Document',
		'set' => 'lithium\data\collection\DocumentSet',
		'relationship' => 'lithium\data\model\Relationship',
		'schema' => 'lithium\data\source\mongo_db\Schema'
	);

	public function connect() {}
	public function disconnect() {}
	public function sources($class = null) {}
	public function describe($entity, $schema = array(), array $meta = array()) {
		return $this->_instance('schema');
	}
	public function create($query, array $options = array()) {}
	public function update($query, array $options = array()) {}
	public function delete($query, array $options = array()) {}

	public $point = 0;
	public $result = null;

	public function read($query = null, array $options = array()) {
		$this->point = 0;
		$this->result = array(
			array('id' => 1, 'name' => 'Joe'),
			array('id' => 2, 'name' => 'Moe'),
			array('id' => 3, 'name' => 'Roe')
		);
	}

	public function getNext() {
		return $this->result[$this->point++];
	}

	public function result($type, $resource, $context) {
		switch ($type) {
			case 'next':
				$result = $resource->hasNext() ? $resource->getNext() : null;
			break;
			case 'close':
				unset($resource);
				$result = null;
				break;
		}
		return $result;
	}

	public function relationship($class, $type, $name, array $options = array()) {
		$key = Inflector::camelize($type === 'belongsTo' ? $name : $class::meta('name'));

		$options += compact('name', 'type', 'key');
		$options['from'] = $class;

		$relationship = $this->_classes['relationship'];
		return new $relationship($options);
	}
}

?>