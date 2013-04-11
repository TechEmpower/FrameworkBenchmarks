<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\source;

use lithium\data\source\Http;

class MockHttpModel extends \lithium\data\Model {

	protected $_meta = array(
		'source' => 'posts',
		'connection' => false
	);

	public static $connection = null;

	protected $_schema = array(
		'id' => array('type' => 'integer', 'key' => 'primary'),
		'author_id' => array('type' => 'integer'),
		'title' => array('type' => 'string', 'length' => 255),
		'body' => array('type' => 'text'),
		'created' => array('type' => 'datetime'),
		'updated' => array('type' => 'datetime')
	);

	public static function &connection() {
		if (static::$connection) {
			return static::$connection;
		}
		$result = new Http();
		return $result;
	}
}

?>