<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data;

class MockCreator extends \lithium\tests\mocks\data\MockBase {

	public static $connection = null;

	protected $_meta = array('connection' => false);

	protected $_schema = array(
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
}

?>