<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\model;

class MockQueryComment extends \lithium\tests\mocks\data\MockBase {

	public static $connection = null;

	public $belongsTo = array('MockQueryPost');

	protected $_meta = array('source' => false, 'connection' => false);

	protected $_schema = array(
		'id' => array('type' => 'integer', 'key' => 'primary'),
		'author_id' => array('type' => 'integer'),
		'comment' => array('type' => 'text'),
		'created' => array('type' => 'datetime'),
		'updated' => array('type' => 'datetime')
	);
}

?>