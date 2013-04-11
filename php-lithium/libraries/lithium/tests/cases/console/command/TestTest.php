<?php
/**
* Lithium: the most rad php framework
*
* @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
* @license       http://opensource.org/licenses/bsd-license.php The BSD License
*/

namespace lithium\tests\cases\console\command;

use lithium\console\command\Test;
use lithium\console\Request;
use lithium\core\Libraries;

class TestTest extends \lithium\test\Unit {

	public $request;

	public $classes = array();

	protected $_backup = array();

	public function setUp() {
		Libraries::cache(false);

		$this->classes = array(
			'response' => 'lithium\tests\mocks\console\MockResponse'
		);
		$this->_backup['cwd'] = getcwd();
		$this->_backup['_SERVER'] = $_SERVER;
		$_SERVER['argv'] = array();

		chdir(LITHIUM_LIBRARY_PATH . '/lithium');

		$this->request = new Request(array('input' => fopen('php://temp', 'w+')));
		$this->request->params = array('library' => 'build_test');
	}

	public function tearDown() {
		$_SERVER = $this->_backup['_SERVER'];
		chdir($this->_backup['cwd']);
	}

	public function skip() {
		$isWin = strtoupper(substr(PHP_OS, 0, 3)) === 'WIN';
		$this->skipIf($isWin, 'The test command needs to be refactored to work on windows.');
	}

	public function testRunWithoutPath() {
		$command = new Test(array(
			'request' => $this->request, 'classes' => $this->classes
		));
		$result = $command->run();
		$this->assertFalse($result);
	}

	public function testRunWithInvalidPath() {
		$command = new Test(array(
			'request' => $this->request, 'classes' => $this->classes
		));
		$path = 'Foobar/lithium/tests/mocks/test/cases/MockTest.php';
		$command->run($path);
		$expected = "Path `.*` not found.\n";
		$result = $command->response->error;
		$this->assertPattern("/{$expected}/", $result);
	}

	public function testRunWithInvalidLibrary() {
		$command = new Test(array(
			'request' => $this->request,
			'classes' => $this->classes
		));
		$command->format = 'foobar';
		$path = LITHIUM_LIBRARY_PATH . '/bob/tests/mocks/test/cases/MockTest.php';
		$command->run($path);
		$expected = "No library found in path `";
		$expected .= LITHIUM_LIBRARY_PATH . "/bob/tests/mocks/test/cases/MockTest.php`.\n";
		$result = $command->response->error;
		$this->assertEqual($expected, $result);
	}

	public function testRunWithInvalidHandler() {
		$command = new Test(array(
			'request' => $this->request,
			'classes' => $this->classes
		));
		$command->format = 'foobar';
		$path = LITHIUM_LIBRARY_PATH . '/lithium/tests/mocks/test/cases/MockTest.php';
		$command->run($path);
		$expected = "No handler for format `foobar`... \n";
		$result = $command->response->error;
		$this->assertEqual($expected, $result);
	}

	public function testRunSingleTestWithAbsolutePath() {
		$command = new Test(array(
			'request' => $this->request, 'classes' => $this->classes
		));
		$path = LITHIUM_LIBRARY_PATH . '/lithium/tests/mocks/test/cases/MockTest.php';
		$command->run($path);

		$expected = "1 pass\n0 fails and 0 exceptions\n";
		$expected = preg_quote($expected);
		$result = $command->response->output;
		$this->assertPattern("/{$expected}/", $result);
	}

	public function testRunSingleTestWithRelativePath() {
		$command = new Test(array(
			'request' => $this->request, 'classes' => $this->classes
		));

		$path = 'tests/mocks/test/cases/MockTest.php';
		$command->run($path);

		$expected = "1 pass\n0 fails and 0 exceptions\n";
		$expected = preg_quote($expected);
		$result = $command->response->output;
		$this->assertPattern("/{$expected}/", $result);

		$command = new Test(array(
			'request' => $this->request, 'classes' => $this->classes
		));

		$current = basename(getcwd());
		$path = "../{$current}/tests/mocks/test/cases/MockTest.php";
		$command->run($path);

		$expected = "1 pass\n0 fails and 0 exceptions\n";
		$expected = preg_quote($expected);
		$result = $command->response->output;
		$this->assertPattern("/{$expected}/", $result);

		$current = basename(getcwd());
		$path = "{$current}/tests/mocks/test/cases/MockTest.php";
		$command->run($path);

		$expected = "1 pass\n0 fails and 0 exceptions\n";
		$expected = preg_quote($expected);
		$result = $command->response->output;
		$this->assertPattern("/{$expected}/", $result);
	}

	public function testRunMultipleTestsWithAbsolutePath() {
		$command = new Test(array(
			'request' => $this->request, 'classes' => $this->classes
		));
		$path = LITHIUM_LIBRARY_PATH . '/lithium/tests/mocks/test/cases';
		$command->run($path);

		$expected = "4 exceptions";
		$expected = preg_quote($expected, '/');
		$result = $command->response->output;
		$this->assertPattern("/{$expected}/", $result);
	}

	public function testReturnRunTestPasses() {
		$command = new Test(array(
			'request' => $this->request, 'classes' => $this->classes
		));
		$path = LITHIUM_LIBRARY_PATH . '/lithium/tests/mocks/test/cases/MockTest.php';
		$result = $command->run($path);
		$this->assertTrue($result);
	}

	public function testReturnRunTestFails() {
		$command = new Test(array(
			'request' => $this->request, 'classes' => $this->classes
		));
		$path = LITHIUM_LIBRARY_PATH . '/lithium/tests/mocks/test/cases/MockTestErrorHandling.php';
		$result = $command->run($path);
		$this->assertFalse($result);
	}

	public function testJsonFormat() {
		$command = new Test(array(
			'request' => $this->request, 'classes' => $this->classes
		));
		$path = LITHIUM_LIBRARY_PATH . '/lithium/tests/mocks/test/cases/MockTest.php';
		$command->format = 'json';
		$command->run($path);

		$result = $command->response->output;
		$result = json_decode($result, true);

		$this->assertTrue(isset($result['count']));
		$this->assertTrue(isset($result['stats']));
	}
}

?>