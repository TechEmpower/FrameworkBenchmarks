<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\template\view;

use stdClass;
use lithium\action\Request;
use lithium\action\Response;
use lithium\template\Helper;
use lithium\template\helper\Html;
use lithium\template\view\adapter\Simple;
use lithium\net\http\Router;
use lithium\template\View;

class RendererTest extends \lithium\test\Unit {

	public function setUp() {
		$this->_routes = Router::get();
		Router::reset();
		Router::connect('/{:controller}/{:action}');
		$this->subject = new Simple(array(
			'request' => new Request(array(
				'base' => '', 'env' => array('HTTP_HOST' => 'foo.local')
			)),
			'response' => new Response()
		));
	}

	public function tearDown() {
		Router::reset();

		foreach ($this->_routes as $route) {
			Router::connect($route);
		}
	}

	public function testInitialization() {
		$expected = array('url', 'path', 'options', 'title', 'value', 'scripts', 'styles', 'head');
		$result = array_keys($this->subject->handlers());
		$this->assertEqual($expected, $result);

		$expected = array('content', 'title', 'scripts', 'styles', 'head');
		$result = array_keys($this->subject->context());
		$this->assertEqual($expected, $result);
	}

	public function testContextQuerying() {
		$expected = array(
			'content' => '', 'title' => '', 'scripts' => array(),
			'styles' => array(), 'head' => array()
		);
		$this->assertEqual($expected, $this->subject->context());
		$this->assertEqual('', $this->subject->context('title'));
		$this->assertEqual(array(), $this->subject->context('scripts'));
		$this->assertEqual(array(), $this->subject->scripts);
		$this->assertNull($this->subject->foo());
		$this->assertFalse(isset($this->subject->foo));

		$result = $this->subject->title("<script>alert('XSS');</script>");
		$this->assertEqual('&lt;script&gt;alert(&#039;XSS&#039;);&lt;/script&gt;', $result);

		$result = $this->subject->title();
		$this->assertEqual('&lt;script&gt;alert(&#039;XSS&#039;);&lt;/script&gt;', $result);

		$this->subject = new Simple(array('context' => array(
			'content' => '', 'title' => '', 'scripts' => array(), 'styles' => array(), 'foo' => '!'
		)));
		$this->assertEqual('!', $this->subject->foo());
		$this->assertTrue(isset($this->subject->foo));
	}

	/**
	 * Tests that URLs are properly escaped by the URL handler.
	 */
	public function testUrlAutoEscaping() {
		Router::connect('/{:controller}/{:action}/{:id}');

		$this->assertEqual('/<foo>/<bar>', $this->subject->url('/<foo>/<bar>'));
		$result = $this->subject->url(array('Controller::action', 'id' => '<script />'));
		$this->assertEqual('/controller/action/<script />', $result);

		$this->subject = new Simple(array(
			'response' => new Response(), 'view' => new View(), 'request' => new Request(array(
				'base' => '', 'env' => array('HTTP_HOST' => 'foo.local')
			))
		));

		$this->assertEqual('/&lt;foo&gt;/&lt;bar&gt;', $this->subject->url('/<foo>/<bar>'));
		$result = $this->subject->url(array('Controller::action', 'id' => '<script />'));
		$this->assertEqual('/controller/action/&lt;script /&gt;', $result);

		$result = $this->subject->url(array('Posts::index', '?' => array(
			'foo' => 'bar', 'baz' => 'dib'
		)));
		$this->assertEqual('/posts?foo=bar&baz=dib', $result);
	}

	/**
	 * Tests built-in content handlers for generating URLs, paths to static assets, and handling
	 * output of elements written to the request context.
	 */
	public function testCoreHandlers() {
		$url = $this->subject->applyHandler(null, null, 'url', array(
			'controller' => 'foo', 'action' => 'bar'
		));
		$this->assertEqual('/foo/bar', $url);

		$helper = new Html();
		$class = get_class($helper);
		$path = $this->subject->applyHandler($helper, "{$class}::script", 'path', 'foo/file');
		$this->assertEqual('/js/foo/file.js', $path);
		$this->assertEqual('/some/generic/path', $this->subject->path('some/generic/path'));
	}

	public function testHandlerInsertion() {
		$this->subject = new Simple(array('context' => array(
			'content' => '', 'title' => '', 'scripts' => array(), 'styles' => array(), 'foo' => '!'
		)));

		$foo = function($value) { return "Foo: {$value}"; };

		$expected = array(
			'url', 'path', 'options', 'title', 'value', 'scripts', 'styles', 'head', 'foo'
		);
		$result = array_keys($this->subject->handlers(compact('foo')));
		$this->assertEqual($expected, $result);

		$result = $this->subject->applyHandler(null, null, 'foo', 'test value');
		$this->assertEqual('Foo: test value', $result);
		$this->assertEqual('Foo: !', $this->subject->foo());

		$this->assertEqual($foo, $this->subject->handlers('foo'));
		$this->assertNull($this->subject->handlers('bar'));

		$bar = function($value) { return "Bar: {$value}"; };

		$this->subject->handlers(compact('bar'));
		$result = $this->subject->applyHandler(null, null, 'bar', 'test value');
		$expected = 'Bar: test value';
		$this->assertEqual($expected, $result);

		$result = $this->subject->bar('test value');
		$this->assertEqual($expected, $result);
	}

	public function testHandlerQuerying() {
		$result = $this->subject->nonExistent('test value');
		$expected = 'test value';
		$this->assertEqual($expected, $result);

		$result = $this->subject->applyHandler(null, null, 'nonExistent', 'test value');
		$this->assertEqual($expected, $result);

		$html = new Html();
		$script = '<script>alert("XSS!");</script>';
		$escaped = '&lt;script&gt;alert(&quot;XSS!&quot;);&lt;/script&gt;';

		$result = $this->subject->applyHandler($html, null, 'title', $script);
		$expected = $escaped;
		$this->assertEqual($expected, $result);

		$result = $this->subject->applyHandler($html, null, 'foo', $script);
		$expected = $script;
		$this->assertEqual($expected, $result);

		$result = $this->subject->applyHandler($html, null, 'bar', $script);
		$this->assertEqual($expected, $result);

		$this->subject->handlers(array('foo' => array($html, 'escape'), 'bar' => 42));

		$result = $this->subject->applyHandler($html, null, 'bar', $script);
		$this->assertEqual($expected, $result);

		$result = $this->subject->applyHandler($html, null, 'foo', $script);
		$expected = $escaped;
		$this->assertEqual($expected, $result);
	}

	public function testHelperLoading() {
		$helper = $this->subject->helper('html');
		$this->assertTrue($helper instanceof Helper);

		$this->expectException('/invalidFoo/');
		$this->assertNull($this->subject->helper('invalidFoo'));
	}

	public function testHelperQuerying() {
		$helper = $this->subject->html;
		$this->assertTrue($helper instanceof Helper);
	}

	public function testTemplateStrings() {
		$result = $this->subject->strings();
		$this->assertTrue(is_array($result));
		$this->assertFalse($result);

		$expected = array('data' => 'The data goes here: {:data}');
		$result = $this->subject->strings($expected);
		$this->assertEqual($expected, $result);

		$result = $this->subject->strings();
		$this->assertEqual($expected, $result);

		$result = $this->subject->strings('data');
		$expected = $expected['data'];
		$this->assertEqual($expected, $result);
	}

	public function testGetters() {
		$this->assertTrue($this->subject->request() instanceof Request);
		$this->assertTrue($this->subject->response() instanceof Response);
		$this->subject = new Simple();
		$this->assertNull($this->subject->request());
		$this->assertNull($this->subject->response());
	}

	public function testSetAndData() {
		$data = array('one' => 1, 'two' => 2, 'three' => 'value');
		$result = $this->subject->set($data);
		$this->assertNull($result);

		$result = $this->subject->data();
		$this->assertEqual($data, $result);

		$result = $this->subject->set(array('more' => new stdClass()));
		$this->assertNull($result);

		$result = $this->subject->data();
		$this->assertEqual($data + array('more' => new stdClass()), $result);
	}

	/**
	 * @todo Add integration test for Renderer being composed with a view object.
	 */
	public function testView() {
		$result = $this->subject->view();
		$this->assertNull($result);
	}

	public function testHandlers() {
		$this->assertTrue($this->subject->url());
		$this->assertPattern('/\/posts\/foo/', $this->subject->url('Posts::foo'));

		$absolute = $this->subject->url('Posts::foo', array('absolute' => true));
		$this->assertEqual('http://foo.local/posts/foo', $absolute);

		$this->assertFalse(trim($this->subject->scripts()));
		$this->assertEqual('foobar', trim($this->subject->scripts('foobar')));
		$this->assertEqual('foobar', trim($this->subject->scripts()));

		$this->assertFalse(trim($this->subject->styles()));
		$this->assertEqual('foobar', trim($this->subject->styles('foobar')));
		$this->assertEqual('foobar', trim($this->subject->styles()));

		$this->assertFalse($this->subject->title());
		$this->assertEqual('Foo', $this->subject->title('Foo'));
		$this->assertEqual('Bar', $this->subject->title('Bar'));
		$this->assertEqual('Bar', $this->subject->title());

		$this->assertFalse(trim($this->subject->head()));
		$this->assertEqual('foo', trim($this->subject->head('foo')));
		$this->assertEqual("foo\n\tbar", trim($this->subject->head('bar')));
		$this->assertEqual("foo\n\tbar", trim($this->subject->head()));
	}

	public function testRespondsTo() {
		$this->assertTrue($this->subject->respondsTo('foobarbaz'));
		$this->assertFalse($this->subject->respondsTo(0));
	}

}

?>