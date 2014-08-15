<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data;

use lithium\tests\mocks\data\source\database\adapter\MockAdapter;

class MockModel extends \lithium\data\Model {

	public static function __init() {}

	public static function &connection($records = null) {
		$mock = new MockAdapter(compact('records') + array(
			'columns' => array(
				'lithium\tests\mocks\data\MockModel' => array('id', 'data')
			),
			'autoConnect' => false
		));
		static::meta(array('key' => 'id', 'locked' => true));
		return $mock;
	}
}

?>