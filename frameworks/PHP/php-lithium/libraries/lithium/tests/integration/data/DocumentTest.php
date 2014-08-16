<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\integration\data;

use lithium\data\Connections;
use lithium\tests\mocks\data\Companies;

class DocumentTest extends \lithium\test\Integration {

	protected $_database;

	protected $_connection = null;

	protected $_key = null;

	/**
	 * Creating the test database
	 */
	public function setUp() {
		$this->_connection->connection->put($this->_database);
	}

	/**
	 * Dropping the test database
	 */
	public function tearDown() {
		$this->_connection->connection->delete($this->_database);
	}

	/**
	 * Skip the test if no test database connection available.
	 */
	public function skip() {
		$connection = 'lithium_couch_test';
		$config = Connections::get($connection, array('config' => true));
		$isConnected = $config && Connections::get($connection)->isConnected(array(
			'autoConnect' => true
		));
		$isAvailable = $config && $isConnected;
		$this->skipIf(!$isAvailable, "No {$connection} connection available.");

		$this->_key = Companies::key();
		$this->_database = $config['database'];
		$this->_connection = Connections::get($connection);
	}

	public function testUpdateWithNewArray() {
		$new = Companies::create(array('name' => 'Acme, Inc.', 'active' => true));

		$expected = array('name' => 'Acme, Inc.', 'active' => true);
		$result = $new->data();
		$this->assertEqual($expected, $result);

		$new->foo = array('bar');
		$expected = array('name' => 'Acme, Inc.', 'active' => true, 'foo' => array('bar'));
		$result = $new->data();
		$this->assertEqual($expected, $result);

		$this->assertTrue($new->save());

		$updated = Companies::find((string) $new->_id);
		$expected = 'bar';
		$result = $updated->foo[0];
		$this->assertEqual($expected, $result);

		$updated->foo[1] = 'baz';

		$this->assertTrue($updated->save());

		$updated = Companies::find((string) $updated->_id);
		$expected = 'baz';
		$result = $updated->foo[1];
		$this->assertEqual($expected, $result);
	}
}

?>