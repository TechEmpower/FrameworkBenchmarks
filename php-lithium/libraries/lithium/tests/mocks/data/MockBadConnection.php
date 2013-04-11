<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data;

class MockBadConnection extends \lithium\data\Model {

	public static $connection = null;

	protected $_meta = array('connection' => 'bad_connection');
}

?>