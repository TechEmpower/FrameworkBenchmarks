<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\model;

class MockTag extends \lithium\tests\mocks\data\MockBase {

	public $hasMany = array(
		'ImageTag' => array('to' => 'lithium\tests\mocks\data\model\MockImageTag')
	);

	public static $connection = null;

	protected $_meta = array(
		'key' => 'id',
		'name' => 'Tag',
		'source' => 'mock_tag',
		'connection' => false
	);

	protected $_schema = array(
		'id' => array('type' => 'integer'),
		'name' => array('type' => 'string')
	);
}

?>