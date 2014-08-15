<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\model;

class MockDatabaseComment extends \lithium\tests\mocks\data\MockBase {

	public static $connection = null;

	public $belongsTo = array('MockDatabasePost');

	protected $_meta = array('connection' => false);

	protected $_schema = array(
		'id' => array('type' => 'integer'),
		'post_id' => array('type' => 'integer'),
		'author_id' => array('type' => 'integer'),
		'body' => array('type' => 'text'),
		'created' => array('type' => 'datetime')
	);
}

?>