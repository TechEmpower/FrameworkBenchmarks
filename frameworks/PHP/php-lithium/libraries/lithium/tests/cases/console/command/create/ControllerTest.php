<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\console\command\create;

use lithium\console\command\create\Controller;
use lithium\console\Request;
use lithium\core\Libraries;

class ControllerTest extends \lithium\test\Unit {

	public $request;

	protected $_backup = array();

	protected $_testPath = null;

	public function skip() {
		$this->_testPath = Libraries::get(true, 'resources') . '/tmp/tests';
		$this->skipIf(!is_writable($this->_testPath), "Path `{$this->_testPath}` is not writable.");
	}

	public function setUp() {
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

	public function testClass() {
		$this->request->params += array(
			'command' => 'controller', 'action' => 'Posts'
		);
		$model = new Controller(array(
			'request' => $this->request, 'classes' => $this->classes
		));

		$expected = 'PostsController';
		$result = $model->invokeMethod('_class', array($this->request));
		$this->assertEqual($expected, $result);
	}

	public function testUse() {
		$this->request->params += array(
			'command' => 'controller', 'action' => 'Posts'
		);
		$model = new Controller(array(
			'request' => $this->request, 'classes' => $this->classes
		));

		$expected = 'create_test\\models\\Posts';
		$result = $model->invokeMethod('_use', array($this->request));
		$this->assertEqual($expected, $result);
	}

	public function testRun() {
		$this->request->params += array(
			'command' => 'create', 'action' => 'controller',
			'args' => array('Posts')
		);
		$controller = new Controller(array(
			'request' => $this->request, 'classes' => $this->classes
		));
		$controller->path = $this->_testPath;
		$controller->run('controller');
		$expected = "PostsController created in controllers/PostsController.php.\n";
		$result = $controller->response->output;
		$this->assertEqual($expected, $result);

		$expected = <<<'test'


namespace create_test\controllers;

use create_test\models\Posts;
use lithium\action\DispatchException;

class PostsController extends \lithium\action\Controller {

	public function index() {
		$posts = Posts::all();
		return compact('posts');
	}

	public function view() {
		$post = Posts::first($this->request->id);
		return compact('post');
	}

	public function add() {
		$post = Posts::create();

		if (($this->request->data) && $post->save($this->request->data)) {
			return $this->redirect(array('Posts::view', 'args' => array($post->id)));
		}
		return compact('post');
	}

	public function edit() {
		$post = Posts::find($this->request->id);

		if (!$post) {
			return $this->redirect('Posts::index');
		}
		if (($this->request->data) && $post->save($this->request->data)) {
			return $this->redirect(array('Posts::view', 'args' => array($post->id)));
		}
		return compact('post');
	}

	public function delete() {
		if (!$this->request->is('post') && !$this->request->is('delete')) {
			$msg = "Posts::delete can only be called with http:post or http:delete.";
			throw new DispatchException($msg);
		}
		Posts::find($this->request->id)->delete();
		return $this->redirect('Posts::index');
	}
}


test;
		$replace = array("<?php", "?>");
		$result = str_replace($replace, '',
			file_get_contents($this->_testPath . '/create_test/controllers/PostsController.php')
		);
		$this->assertEqual($expected, $result);
	}
}

?>