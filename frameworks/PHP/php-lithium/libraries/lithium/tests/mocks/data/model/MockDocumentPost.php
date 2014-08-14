<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\model;

use lithium\data\entity\Document;
use lithium\data\collection\DocumentSet;

class MockDocumentPost extends \lithium\tests\mocks\data\MockBase {

	protected $_meta = array('connection' => false, 'initialized' => true, 'key' => '_id');

	protected static $_connection;

	public static function __init() {}

	public static function schema($field = null) {
		$schema = parent::schema();
		$schema->append(array(
			'_id' => array('type' => 'id'),
			'foo' => array('type' => 'object'),
			'foo.bar' => array('type' => 'int')
		));
		return $schema;
	}

	public function ret($record, $param1 = null, $param2 = null) {
		if ($param2) {
			return $param2;
		}
		if ($param1) {
			return $param1;
		}
		return null;
	}

	public function medicin($record) {
		return 'lithium';
	}

	public static function find($type = 'all', array $options = array()) {
		switch ($type) {
			case 'first':
				return new Document(array(
					'data' => array('_id' => 2, 'name' => 'Two', 'content' => 'Lorem ipsum two'),
					'model' => __CLASS__
				));
			break;
			case 'all':
			default:
				return new DocumentSet(array(
					'data' => array(
						array('_id' => 1, 'name' => 'One', 'content' => 'Lorem ipsum one'),
						array('_id' => 2, 'name' => 'Two', 'content' => 'Lorem ipsum two'),
						array('_id' => 3, 'name' => 'Three', 'content' => 'Lorem ipsum three')
					),
					'model' => __CLASS__
				));
			break;
		}
	}
}

?>