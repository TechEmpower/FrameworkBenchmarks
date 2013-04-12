<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\source\http\adapter;

use lithium\data\DocumentSchema;

class MockCouchPost extends \lithium\tests\mocks\data\MockBase {

	public static $connection;

	protected $_meta = array('source' => 'posts', 'connection' => false, 'key' => 'id');

	protected $_schema = array(
		'id' => array('type' => 'integer'),
		'author_id' => array('type' => 'integer'),
		'title' => array('type' => 'string', 'length' => 255),
		'body' => array('type' => 'text'),
		'created' => array('type' => 'datetime'),
		'updated' => array('type' => 'datetime')
	);

	public static function resetSchema($array = false) {
		if ($array) {
			return static::_object()->_schema = array();
		}
		static::_object()->_schema = new DocumentSchema();
	}
}

?>