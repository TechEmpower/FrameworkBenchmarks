<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\model;

class MockDatabasePost extends \lithium\tests\mocks\data\MockBase {

	public static $connection = null;

	public $hasMany = array(
		'MockDatabaseComment',
		'MockDatabasePostRevision' => array(
			'constraints' => array('MockDatabasePostRevision.deleted' => null)
		)
	);

	protected $_meta = array('connection' => false, 'key' => 'id');

	protected $_schema = array(
		'id' => array('type' => 'integer'),
		'author_id' => array('type' => 'integer'),
		'title' => array('type' => 'string'),
		'created' => array('type' => 'datetime')
	);
}

?>