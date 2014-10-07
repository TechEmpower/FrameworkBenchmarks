<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2009, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\model;

class MockDatabaseTagging extends \lithium\tests\mocks\data\MockBase {

	public static $connection = null;

	public $belongsTo = array('MockDatabasePost', 'MockDatabaseTag');

	protected $_meta = array('connection' => false);

	protected $_schema = array(
		'id' => array('type' => 'integer'),
		'post_id' => array('type' => 'integer'),
		'tag_id' => array('type' => 'integer')
	);
}

?>