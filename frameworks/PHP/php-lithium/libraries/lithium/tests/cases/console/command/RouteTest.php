<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\console\command;

use lithium\console\command\Route;
use lithium\console\Request;
use lithium\net\http\Router;
use lithium\core\Libraries;

/**
 * The RouteTest class tests the "li3 route" command.
 */
class RouteTest extends \lithium\test\Unit {

	/**
	 * Holds config params.
	 *
	 * @var array
	 */
	protected $_config = array('routes' => null);

	/**
	 * Holds the temporary test path.
	 *
	 * @var string
	 */
	protected $_testPath = null;

	/**
	 * Set the testPath and check if it is writable (skip if not).
	 */
	public function skip() {
		$path = Libraries::get(true, 'resources');

		if (is_writable($path) && !is_dir("{$path}/tmp/tests")) {
			mkdir("{$path}/tmp/tests", 0777, true);
		}
		$this->_testPath = "{$path}/tmp/tests";
		$this->skipIf(!is_writable($this->_testPath), "Path `{$this->_testPath}` is not writable.");
	}

	/**
	 * Create a temporary routes.php file for testing and reset the router.
	 */
	public function setUp() {
		$this->_config['routes'] = "{$this->_testPath}/routes.php";

		$testParams = 'array("controller" => "lithium\test\Controller")';
		$content = array(
			'<?php',
			'use lithium\net\http\Router;',
			'use lithium\core\Environment;',
			'Router::connect("/", "Pages::view");',
			'Router::connect("/pages/{:args}", "Pages::view");',
			'if (!Environment::is("production")) {',
				'Router::connect("/test/{:args}", ' . $testParams . ');',
				'Router::connect("/test", ' . $testParams . ');',
			'}',
			'?>'
		);
		file_put_contents($this->_config['routes'], join("\n", $content));

		Router::reset();
	}

	/**
	 * Delete the temporary routes.php file.
	 */
	public function tearDown() {
		if (file_exists($this->_config['routes'])) {
			unlink($this->_config['routes']);
		}
	}

	/**
	 * Tests if the default environment is loaded correctly
	 * and if overriding works as expected.
	 */
	public function testEnvironment() {
		$command = new Route(array('routes' => $this->_config['routes']));
		$expected = 'development';
		$this->assertEqual($expected, $command->env);

		$request = new Request();
		$request->params['env'] = 'production';
		$command = new Route(compact('request') + array('routes' => $this->_config['routes']));
		$this->assertEqual('production', $command->env);
	}

	/**
	 * Test if the routes.php file is loaded correctly and the
	 * routes are connected to the router.
	 */
	public function testRouteLoading() {
		$this->assertFalse(Router::get());

		$command = new Route(array('routes' => $this->_config['routes']));
		$this->assertEqual(4, count(Router::get()));

		Router::reset();

		$request = new Request();
		$request->params['env'] = 'production';
		$command = new Route(compact('request') + array('routes' => $this->_config['routes']));
		$this->assertEqual(2, count(Router::get()));
	}

	/**
	 * Tests the "all" command without an env param.
	 *
	 * Don't be confused if the expected output doesn't make sense here. We are
	 * stripping the whitespace away so that this source code is easier to read.
	 * Built-In methods are used for output formatting and are tested elsewhere.
	 */
	public function testAllWithoutEnvironment() {
		$command = new Route(array(
			'routes' => $this->_config['routes'],
			'classes' => array('response' => 'lithium\tests\mocks\console\MockResponse'),
			'request' => new Request()
		));

		$command->all();

		$expected = 'TemplateParams--------------
			/{"controller":"Pages","action":"view"}
			/pages/{:args}{"controller":"Pages","action":"view"}
			/test/{:args}{"controller":"lithium\\test\\\\Controller","action":"index"}
			/test{"controller":"lithium\\test\\\\Controller","action":"index"}';
		$this->assertEqual($this->_strip($expected),$this->_strip($command->response->output));
	}

	/**
	 * Tests the "all" command with an env (production) param.
	 *
	 * Don't be confused if the expected output doesn't make sense here. We are
	 * stripping the whitespace away so that this source code is easier to read.
	 * Built-In methods are used for output formatting and are tested elsewhere.
	 */
	public function testAllWithEnvironment() {
		$request = new Request();
		$request->params = array(
			'env' => 'production'
		);
		$command = new Route(compact('request') + array(
			'routes' => $this->_config['routes'],
			'classes' => array('response' => 'lithium\tests\mocks\console\MockResponse')
		));

		$command->all();

		$expected = 'TemplateParams--------------
			/{"controller":"Pages","action":"view"}
			/pages/{:args}{"controller":"Pages","action":"view"}';
		$this->assertEqual($this->_strip($expected),$this->_strip($command->response->output));
	}

	/**
	 * Test the alias method for "all".
	 */
	public function testRun() {
		$command = new Route(array(
			'routes' => $this->_config['routes'],
			'classes' => array('response' => 'lithium\tests\mocks\console\MockResponse'),
			'request' => new Request()
		));

		$command->run();

		$expected = 'TemplateParams--------------
			/{"controller":"Pages","action":"view"}
			/pages/{:args}{"controller":"Pages","action":"view"}
			/test/{:args}{"controller":"lithium\\test\\\\Controller","action":"index"}
			/test{"controller":"lithium\\test\\\\Controller","action":"index"}';
		$this->assertEqual($this->_strip($expected),$this->_strip($command->response->output));
	}

	/**
	 * Test the show command with no route.
	 */
	public function testShowWithNoRoute() {
		$command = new Route(array(
			'routes' => $this->_config['routes'],
			'classes' => array('response' => 'lithium\tests\mocks\console\MockResponse'),
			'request' => new Request()
		));

		$command->show();

		$expected = "Please provide a valid URL\n";
		$this->assertEqual($expected, $command->response->error);
	}

	/**
	 * Test the show command with an invalid route.
	 */
	public function testShowWithInvalidRoute() {
		$request = new Request();
		$request->params = array(
			'args' => array('/foobar')
		);
		$command = new Route(compact('request') + array(
			'routes' => $this->_config['routes'],
			'classes' => array('response' => 'lithium\tests\mocks\console\MockResponse')
		));
		$command->show();

		$expected = "No route found.\n";
		$this->assertEqual($expected, $command->response->output);
	}

	/**
	 * Test the show command with a valid route.
	 */
	public function testShowWithValidRoute() {
		$request = new Request();
		$request->params = array('args' => array('/'));
		$command = new Route(compact('request') + array(
			'routes' => $this->_config['routes'],
			'classes' => array('response' => 'lithium\tests\mocks\console\MockResponse')
		));
		$command->show();

		$expected = "{\"controller\":\"Pages\",\"action\":\"view\"}\n";
		$this->assertEqual($expected, $command->response->output);
	}

	/**
	 * Test the show command with a env param.
	 */
	public function testShowWithEnvironment() {
		$request = new Request();
		$request->params = array(
			'env' => 'production',
			'args' => array('/test')
		);
		$command = new Route(compact('request') + array(
			'routes' => $this->_config['routes'],
			'classes' => array('response' => 'lithium\tests\mocks\console\MockResponse')
		));

		$command->show();

		$expected = "No route found.\n";
		$this->assertEqual($expected, $command->response->output);
	}

	/**
	 * Test the show command with http method.
	 *
	 * This tests a call similar to "li3 route GET /".
	 */
	public function testShowWithHttpMethod() {
		$request = new Request();
		$request->params = array(
			'args' => array('post', '/')
		);
		$command = new Route(compact('request') + array(
			'routes' => $this->_config['routes'],
			'classes' => array('response' => 'lithium\tests\mocks\console\MockResponse')
		));

		$command->show();

		$expected = "{\"controller\":\"Pages\",\"action\":\"view\"}\n";
		$this->assertEqual($expected, $command->response->output);
	}

	/**
	 * Remove formatting whitespace, tabs and newlines for better sourcecode
	 * readability.
	 *
	 * @param string $str A string from which to strip spaces
	 * @return string Returns the value of `$str` with all whitespace removed.
	 */
	protected function _strip($str) {
		return preg_replace('/\s/', '', $str);
	}
}

?>