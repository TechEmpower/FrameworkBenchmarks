<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD(http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data;

use lithium\tests\mocks\data\model\MockDatabase;

class MockBase extends \lithium\data\Model {

	protected $_meta = array('connection' => null);

	public static $connection = null;

	public static function &connection() {
		if (!static::$connection) {
			$connection = new MockDatabase();
			return $connection;
		}
		return static::$connection;
	}
}

?>