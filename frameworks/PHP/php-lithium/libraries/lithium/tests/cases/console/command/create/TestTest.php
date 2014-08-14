<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\console\command\create;

use lithium\console\command\create\Test;
use lithium\console\Request;
use lithium\core\Libraries;

class TestTest extends \lithium\test\Unit {

	public $request;

	protected $_backup = array();

	protected $_testPath = null;

	public function skip() {
		$this->_testPath = Libraries::get(true, 'resources') . '/tmp/tests';
		$this->skipIf(!is_writable($this->_testPath), "Path `{$this->_testPath}` is not writable.");
	}

	public function setUp() {
		Libraries::cache(false);
		$this->classes = array('response' => 'lithium\tests\mocks\console\MockResponse');
		$this->_backup['cwd'] = getcwd();
		$this->_backup['_SERVER'] = $_SERVER;
		$_SERVER['argv'] = array();

		Libraries::add('create_test', array('path' => $this->_testPath . '/create_test'));
		$this->request = new Request(array('input' => fopen('php://temp', 'w+')));
		$this->request->params = array('library' => 'create_test');
	}

	public function tearDown() {
		$_SERVER = $this->_backup['_SERVER'];
		chdir($this->_backup['cwd']);
		$this->_cleanUp();
	}

	public function testTestModel() {
		$this->request->params += array(
			'command' => 'create', 'action' => 'test',
			'args' => array('model', 'Posts')
		);
		$test = new Test(array(
			'request' => $this->request, 'classes' => $this->classes
		));
		$test->path = $this->_testPath;
		$test->run('test');
		$expected = "PostsTest created in tests/cases/models/PostsTest.php.\n";
		$result = $test->response->output;
		$this->assertEqual($expected, $result);

		$expected = <<<'test'


namespace create_test\tests\cases\models;

use create_test\models\Posts;

class PostsTest extends \lithium\test\Unit {

	public function setUp() {}

	public function tearDown() {}


}


test;
		$replace = array("<?php", "?>");
		$result = str_replace($replace, '',
			file_get_contents($this->_testPath . '/create_test/tests/cases/models/PostsTest.php')
		);
		$this->assertEqual($expected, $result);
	}

	public function testTestModelWithMethods() {
		$this->_cleanUp();
		mkdir($this->_testPath . '/create_test/models/', 0755, true);
		$id = rand();
		$path = "create_test/models/Post{$id}s.php";
		file_put_contents("{$this->_testPath}/{$path}",
"<?php
namespace create_test\models;

class Post{$id}s {
	public function someMethod() {}
}"
);

		$this->request->params += array('command' => 'create', 'action' => 'test', 'args' => array(
			'model', "Post{$id}s"
		));
		$test = new Test(array('request' => $this->request, 'classes' => $this->classes));
		$test->path = $this->_testPath;
		$test->run('test');
		$expected = "Post{$id}sTest created in tests/cases/models/Post{$id}sTest.php.\n";
		$result = $test->response->output;
		$this->assertEqual($expected, $result);

		$expected = <<<test


namespace create_test\\tests\\cases\\models;

use create_test\\models\\Post{$id}s;

class Post{$id}sTest extends \\lithium\\test\\Unit {

	public function setUp() {}

	public function tearDown() {}

	public function testSomeMethod() {}
}


test;
		$replace = array("<?php", "?>");
		$path = "create_test/tests/cases/models/Post{$id}sTest.php";
		$result = str_replace($replace, '', file_get_contents("{$this->_testPath}/{$path}"));
		$this->assertEqual($expected, $result);
	}
}

?>