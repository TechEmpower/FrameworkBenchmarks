<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\model;

class MockImage extends \lithium\tests\mocks\data\MockBase {

	public $belongsTo = array(
		'Gallery' => array('to' => 'lithium\tests\mocks\data\model\MockGallery')
	);

	public $hasMany = array(
		'ImageTag' => array('to' => 'lithium\tests\mocks\data\model\MockImageTag')
	);

	public static $connection = null;

	protected $_meta = array(
		'key' => 'id',
		'name' => 'Image',
		'source' => 'mock_image',
		'connection' => false
	);

	protected $_schema = array(
		'id' => array('type' => 'integer'),
		'title' => array('type' => 'string'),
		'image' => array('type' => 'string'),
		'gallery_id' => array('type' => 'integer')
	);
}

?>