<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD(http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data;

class MockPost extends \lithium\tests\mocks\data\MockBase {

	public $hasMany = array('MockComment');

	public static $connection = null;

	protected $_meta = array('connection' => false, 'key' => 'id');

	public static function instances() {
		return array_keys(static::$_instances);
	}

	public function foobar() {
		return;
	}

}

?>