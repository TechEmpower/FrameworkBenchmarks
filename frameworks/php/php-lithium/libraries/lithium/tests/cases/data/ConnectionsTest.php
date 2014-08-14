<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\data;

use Exception;
use lithium\data\Connections;
use lithium\data\source\Http;
use lithium\data\source\Mock;
use lithium\data\source\database\adapter\MySql;
use lithium\data\source\database\adapter\PostgreSql;

class ConnectionsTest extends \lithium\test\Unit {

	public $config = array(
		'type'     => 'Mock',
		'adapter'  => null,
		'host'     => 'localhost',
		'login'    => '--user--',
		'password' => '--pass--',
		'database' => 'db'
	);

	protected $_port = null;

	protected $_backup = array();

	public function setUp() {
		if (!$this->_backup) {
			foreach (Connections::get() as $conn) {
				$this->_backup[$conn] = Connections::get($conn, array('config' => true));
			}
		}
		Connections::reset();
	}

	public function tearDown() {
		Connections::reset();
		Connections::config($this->_backup);
	}

	public function testConnectionCreate() {
		$result = Connections::add('conn-test', array('type' => 'Mock') + $this->config);
		$expected = $this->config + array('type' => 'Mock');
		$this->assertEqual($expected, $result);

		$result = Connections::get('conn-test');
		$this->assertTrue($result instanceof Mock);

		$result = Connections::add('conn-test-2', $this->config);
		$this->assertEqual($expected, $result);

		$result = Connections::get('conn-test-2');
		$this->assertTrue($result instanceof Mock);
	}

	public function testConnectionGetAndReset() {
		Connections::add('conn-test', $this->config);
		Connections::add('conn-test-2', $this->config);
		$this->assertEqual(array('conn-test', 'conn-test-2'), Connections::get());

		$enabled = (MySql::enabled() || PostgreSql::enabled());
		$this->skipIf(!$enabled, 'MySql or PostgreSQL is not enabled');

		if (MySql::enabled()) {
			$this->_port = 3306;
		}
		if (PostgreSql::enabled()) {
			$this->_port = 5432;
		}

		$msg = "Cannot connect to localhost:{$this->_port}";
		$this->skipIf(!$this->_canConnect('localhost', $this->_port), $msg);

		$expected = $this->config + array('type' => 'database', 'filters' => array());
		$this->assertEqual($expected, Connections::get('conn-test', array('config' => true)));

		$this->assertNull(Connections::reset());
		$this->assertFalse(Connections::get());

		$this->assertTrue(is_array(Connections::get()));
	}

	public function testConnectionAutoInstantiation() {
		Connections::add('conn-test', $this->config);
		Connections::add('conn-test-2', $this->config);

		$result = Connections::get('conn-test');
		$this->assertTrue($result instanceof Mock);

		$result = Connections::get('conn-test');
		$this->assertTrue($result instanceof Mock);

		$this->assertNull(Connections::get('conn-test-2', array('autoCreate' => false)));
	}

	public function testInvalidConnection() {
		$this->assertNull(Connections::get('conn-invalid'));
	}

	public function testStreamConnection() {
		$config = array(
			'type' => 'Http',
			'socket' => 'Stream',
			'host' => 'localhost',
			'login' => 'root',
			'password' => '',
			'port' => '80'
		);

		Connections::add('stream-test', $config);
		$result = Connections::get('stream-test');
		$this->assertTrue($result instanceof Http);
		Connections::config(array('stream-test' => false));
	}

	public function testErrorExceptions() {
		$config = array(
			'adapter' => 'None',
			'type' => 'Error'
		);
		Connections::add('NoConnection', $config);
		$result = false;

		try {
			Connections::get('NoConnection');
		} catch(Exception $e) {
			$result = true;
		}
		$this->assertTrue($result, 'Exception is not thrown');
	}

	public function testGetNullAdapter() {
		Connections::reset();
		$this->assertTrue(Connections::get(false) instanceof Mock);
	}

	protected function _canConnect($host, $port) {
		$success = false;
		set_error_handler(function() {});

		if ($conn = fsockopen($host, $port)) {
			fclose($conn);
			$success = true;
		}
		restore_error_handler();
		return $success;
	}
}

?>