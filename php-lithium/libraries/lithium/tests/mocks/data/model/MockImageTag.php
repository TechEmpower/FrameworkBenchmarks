<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\model;

class MockImageTag extends \lithium\tests\mocks\data\MockBase {

	public $belongsTo = array(
		'Image' => array('to' => 'lithium\tests\mocks\data\model\MockImage'),
		'Tag' => array('to' => 'lithium\tests\mocks\data\model\MockTag'));

	public static $connection = null;

	protected $_meta = array(
		'key' => 'id',
		'name' => 'ImageTag',
		'source' => 'mock_image_tag',
		'connection' => false
	);

	protected $_schema = array(
		'id' => array('type' => 'integer'),
		'image_id' => array('type' => 'integer'),
		'tag_id' => array('type' => 'integer')
	);
}

?>