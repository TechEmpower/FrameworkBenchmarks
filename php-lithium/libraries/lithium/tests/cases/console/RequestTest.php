<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\console;

use lithium\core\Libraries;
use lithium\console\Request;

class RequestTest extends \lithium\test\Unit {

	public $streams;

	protected $_backup = array();

	public function setUp() {
		$this->streams = array(
			'input' => Libraries::get(true, 'resources') . '/tmp/tests/input.txt'
		);

		$this->_backup['cwd'] = getcwd();
		$this->_backup['_SERVER'] = $_SERVER;
		$_SERVER['argv'] = array();
	}

	public function tearDown() {
		foreach ($this->streams as $path) {
			if (file_exists($path)) {
				unlink($path);
			}
		}
		$_SERVER = $this->_backup['_SERVER'];
		chdir($this->_backup['cwd']);
	}

	public function testConstructWithoutConfig() {
		$request = new Request();

		$expected = array();
		$result = $request->args;
		$this->assertEqual($expected, $result);

		$result = $request->env();
		$this->assertTrue(!empty($result));

		$expected = getcwd();
		$result = $result['working'];
		$this->assertEqual($expected, $result);
	}

	public function testEnvWorking() {
		$base = Libraries::get(true, 'resources') . '/tmp/tests';
		$this->skipIf(!is_readable($base), "Path `{$base}` is not readable.");

		chdir(Libraries::get(true, 'resources') . '/tmp/tests');
		$request = new Request();

		$expected = realpath(Libraries::get(true, 'resources') . '/tmp/tests');
		$result = $request->env('working');
		$this->assertEqual($expected, $result);
	}

	public function testConstructWithServer() {
		$_SERVER['argv'] = array('/path/to/lithium.php', 'one', 'two');
		$request = new Request();

		$expected = '/path/to/lithium.php';
		$result = $request->env('script');
		$this->assertEqual($expected, $result);

		$expected = array('one', 'two');
		$result = $request->argv;
		$this->assertEqual($expected, $result);
	}

	public function testConstructWithConfigArgv() {
		$request = new Request(array('args' => array('/path/to/lithium.php', 'wrong')));

		$expected = array('/path/to/lithium.php', 'wrong');
		$result = $request->argv;
		$this->assertEqual($expected, $result);

		$_SERVER['argv'] = array('/path/to/lithium.php');
		$request = new Request(array('args' => array('one', 'two')));

		$expected = '/path/to/lithium.php';
		$result = $request->env('script');
		$this->assertEqual($expected, $result);

		$expected = array('one', 'two');
		$result = $request->argv;
		$this->assertEqual($expected, $result);
	}

	public function testConstructWithConfigArgs() {
		$request = new Request(array(
			'args' => array('ok')
		));
		$expected = array('ok');
		$this->assertEqual($expected, $request->argv);

		$request = new Request(array(
			'env' => array('script' => '/path/to/lithium.php'),
			'args' => array('one', 'two', 'three', 'four')
		));

		$expected = '/path/to/lithium.php';
		$result = $request->env('script');
		$this->assertEqual($expected, $result);

		$expected = array('one', 'two', 'three', 'four');
		$this->assertEqual($expected, $request->argv);
	}

	public function testConstructWithEnv() {
		$base = Libraries::get(true, 'resources') . '/tmp/tests';
		$this->skipIf(!is_readable($base), "Path `{$base}` is not readable.");

		chdir(Libraries::get(true, 'resources') . '/tmp');
		$request = new Request(array('env' => array('working' => '/some/other/path')));

		$expected = '/some/other/path';
		$result = $request->env('working');
		$this->assertEqual($expected, $result);
	}

	public function testInput() {
		$base = Libraries::get(true, 'resources') . '/tmp/tests';
		$this->skipIf(!is_writable($base), "{$base} is not writable.");

		$stream = fopen($this->streams['input'], 'w+');
		$request = new Request(array('input' => $stream));
		$this->assertTrue(is_resource($request->input));
		$this->assertEqual($stream, $request->input);

		$this->assertEqual(2, fwrite($request->input, 'ok'));
		rewind($request->input);

		$this->assertEqual('ok', $request->input());
	}

	public function testArgs() {
		$request = new Request();
		$request->params = array(
			'command' => 'one', 'action' => 'two', 'args' => array('three', 'four', 'five')
		);
		$this->assertEqual('five', $request->args(2));
	}

	public function testShiftDefaultOne() {
		$request = new Request();
		$request->params = array(
			'command' => 'one', 'action' => 'two',
			'args' => array('three', 'four', 'five')
		);
		$request->shift();

		$expected = array('command' => 'two', 'action' => 'three', 'args' => array('four', 'five'));
		$this->assertEqual($expected, $request->params);
	}

	public function testShiftTwo() {
		$request = new Request();
		$request->params = array(
			'command' => 'one', 'action' => 'two',
			'args' => array('three', 'four', 'five')
		);
		$request->shift(2);

		$expected = array('command' => 'three', 'action' => 'four', 'args' => array('five'));
		$result = $request->params;
		$this->assertEqual($expected, $result);
	}

	public function testTemporaryFileStructureExists() {
		$resources = Libraries::get(true, 'resources');
		$template = $resources . '/tmp/cache/templates/';
		$this->assert(is_dir($template));
	}
}

?>