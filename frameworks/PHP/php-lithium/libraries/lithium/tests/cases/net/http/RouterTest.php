<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\net\http;

use lithium\action\Request;
use lithium\net\http\Route;
use lithium\net\http\Router;
use lithium\action\Response;

class RouterTest extends \lithium\test\Unit {

	public $request = null;

	protected $_routes = array();

	public function setUp() {
		$this->request = new Request();
		$this->_routes = Router::get();
		Router::reset();
	}

	public function tearDown() {
		Router::reset();

		foreach ($this->_routes as $route) {
			Router::connect($route);
		}
	}

	public function testBasicRouteConnection() {
		$result = Router::connect('/hello', array('controller' => 'posts', 'action' => 'index'));
		$expected = array(
			'template' => '/hello',
			'pattern' => '@^/hello$@u',
			'params' => array('controller' => 'Posts', 'action' => 'index'),
			'match' => array('controller' => 'Posts', 'action' => 'index'),
			'meta' => array(),
			'persist' => array('controller'),
			'defaults' => array(),
			'keys' => array(),
			'subPatterns' => array(),
			'handler' => null
		);
		$this->assertEqual($expected, $result->export());

		$result = Router::connect('/{:controller}/{:action}', array('action' => 'view'));
		$this->assertTrue($result instanceof Route);
		$expected = array(
			'template' => '/{:controller}/{:action}',
			'pattern' => '@^(?:/(?P<controller>[^\\/]+))(?:/(?P<action>[^\\/]+)?)?$@u',
			'params' => array('action' => 'view'),
			'defaults' => array('action' => 'view'),
			'match' => array(),
			'meta' => array(),
			'persist' => array('controller'),
			'keys' => array('controller' => 'controller', 'action' => 'action'),
			'subPatterns' => array(),
			'handler' => null
		);
		$this->assertEqual($expected, $result->export());
	}

	/**
	 * Tests generating routes with required parameters which are not present in the URL.
	 */
	public function testConnectingWithRequiredParams() {
		$result = Router::connect('/{:controller}/{:action}', array(
			'action' => 'view', 'required' => true
		));
		$expected = array(
			'template' => '/{:controller}/{:action}',
			'pattern' => '@^(?:/(?P<controller>[^\\/]+))(?:/(?P<action>[^\\/]+)?)?$@u',
			'keys' => array('controller' => 'controller', 'action' => 'action'),
			'params' => array('action' => 'view', 'required' => true),
			'defaults' => array('action' => 'view'),
			'match' => array('required' => true),
			'meta' => array(),
			'persist' => array('controller'),
			'subPatterns' => array(),
			'handler' => null
		);
		$this->assertEqual($expected, $result->export());
	}

	public function testConnectingWithDefaultParams() {
		$result = Router::connect('/{:controller}/{:action}', array('action' => 'archive'));
		$expected = array(
			'template' => '/{:controller}/{:action}',
			'pattern' => '@^(?:/(?P<controller>[^\/]+))(?:/(?P<action>[^\/]+)?)?$@u',
			'keys' => array('controller' => 'controller', 'action' => 'action'),
			'params' => array('action' => 'archive'),
			'match' => array(),
			'meta' => array(),
			'persist' => array('controller'),
			'defaults' => array('action' => 'archive'),
			'subPatterns' => array(),
			'handler' => null
		);
		$this->assertEqual($expected, $result->export());
	}

	/**
	 * Tests basic options for connecting routes.
	 */
	public function testBasicRouteMatching() {
		Router::connect('/hello', array('controller' => 'posts', 'action' => 'index'));
		$expected = array('controller' => 'Posts', 'action' => 'index');

		foreach (array('/hello/', '/hello', 'hello/', 'hello') as $url) {
			$this->request->url = $url;
			$result = Router::parse($this->request);
			$this->assertEqual($expected, $result->params);
			$this->assertEqual(array('controller'), $result->persist);
		}
	}

	public function testRouteMatchingWithDefaultParameters() {
		Router::connect('/{:controller}/{:action}', array('action' => 'view'));
		$expected = array('controller' => 'posts', 'action' => 'view');

		foreach (array('/posts/view', '/posts', 'posts', 'posts/view', 'posts/view/') as $url) {
			$this->request->url = $url;
			$result = Router::parse($this->request);
			$this->assertEqual($expected, $result->params);
			$this->assertEqual(array('controller'), $result->persist);
		}
		$expected['action'] = 'index';

		foreach (array('/posts/index', 'posts/index', 'posts/index/') as $url) {
			$this->request->url = $url;
			$result = Router::parse($this->request);
			$this->assertEqual($expected, $result->params);
		}

		$this->request->url = '/posts/view/1';
		$this->assertNull(Router::parse($this->request));
	}

	/**
	 * Tests that URLs specified as "Controller::action" are interpreted properly.
	 */
	public function testStringActions() {
		Router::connect('/login', array('controller' => 'sessions', 'action' => 'create'));
		Router::connect('/{:controller}/{:action}');

		$result = Router::match("Sessions::create");
		$this->assertEqual('/login', $result);

		$result = Router::match("Posts::index");
		$this->assertEqual('/posts', $result);

		$result = Router::match("ListItems::archive");
		$this->assertEqual('/list_items/archive', $result);
	}

	public function testNamedAnchor() {
		Router::connect('/{:controller}/{:action}');
		Router::connect('/{:controller}/{:action}/{:id:[0-9]+}', array('id' => null));

		$result = Router::match(array('Posts::edit', '#' => 'foo'));
		$this->assertEqual('/posts/edit#foo', $result);

		$result = Router::match(array('Posts::edit', 'id' => 42, '#' => 'foo'));
		$this->assertEqual('/posts/edit/42#foo', $result);

		$result = Router::match(array('controller' => 'users', 'action' => 'view', '#' => 'blah'));
		$this->assertEqual('/users/view#blah', $result);

		$result = Router::match(array(
			'controller' => 'users', 'action' => 'view', 'id' => 47, '#' => 'blargh'
		));
		$this->assertEqual('/users/view/47#blargh', $result);
	}

	public function testQueryString() {
		Router::connect('/{:controller}/{:action}');
		Router::connect('/{:controller}/{:action}/{:id:[0-9]+}', array('id' => null));

		$result = Router::match(array('Posts::edit', '?' => array('key' => 'value')));
		$this->assertEqual('/posts/edit?key=value', $result);

		$result = Router::match(array(
			'Posts::edit', 'id' => 42, '?' => array('key' => 'value', 'test' => 'foo')
		));
		$this->assertEqual('/posts/edit/42?key=value&test=foo', $result);
	}

	/**
	 * Tests that URLs specified as "Controller::action" and including additional parameters are
	 * interpreted properly.
	 */
	public function testEmbeddedStringActions() {
		Router::connect('/logout/{:id:[0-9]{5,6}}', array(
			'controller' => 'sessions', 'action' => 'destroy', 'id' => null
		));
		Router::connect('/{:controller}/{:action}');
		Router::connect('/{:controller}/{:action}/{:id:[0-9]+}', array('id' => null));

		$result = Router::match("Sessions::create");
		$this->assertEqual('/sessions/create', $result);

		$result = Router::match(array("Sessions::create"));
		$this->assertEqual('/sessions/create', $result);

		$result = Router::match(array("Sessions::destroy", 'id' => '03815'));
		$this->assertEqual('/logout/03815', $result);

		$result = Router::match("Posts::index");
		$this->assertEqual('/posts', $result);

		$ex = "No parameter match found for URL ";
		$ex .= "`('controller' => 'Sessions', 'action' => 'create', 'id' => 'foo')`.";
		$this->expectException($ex);
		$result = Router::match(array("Sessions::create", 'id' => 'foo'));
	}

	/**
	 * Tests that routes can be created with shorthand strings, i.e. `'Controller::action'` and
	 * `array('Controller::action', 'id' => '...')`.
	 */
	public function testStringParameterConnect() {
		Router::connect('/posts/{:id:[0-9a-f]{24}}', 'Posts::edit');

		$result = Router::match(array(
			'controller' => 'posts', 'action' => 'edit', 'id' => '4bbf25bd8ead0e5180130000'
		));
		$expected = '/posts/4bbf25bd8ead0e5180130000';
		$this->assertEqual($expected, $result);

		$ex = "No parameter match found for URL `(";
		$ex .= "'controller' => 'Posts', 'action' => 'view', 'id' => '4bbf25bd8ead0e5180130000')`.";
		$this->expectException($ex);
		$result = Router::match(array(
			'controller' => 'posts', 'action' => 'view', 'id' => '4bbf25bd8ead0e5180130000'
		));
		$this->assertFalse(ob_get_length());
	}

	public function testShorthandParameterMatching() {
		Router::reset();
		Router::connect('/posts/{:page:[0-9]+}', array('Posts::index', 'page' => '1'));

		$result = Router::match(array('controller' => 'posts', 'page' => '5'));
		$expected = '/posts/5';
		$this->assertEqual($expected, $result);

		$result = Router::match(array('Posts::index', 'page' => '10'));
		$expected = '/posts/10';
		$this->assertEqual($expected, $result);

		$request = new Request(array('url' => '/posts/13'));
		$result = Router::process($request);
		$expected = array('controller' => 'Posts', 'action' => 'index', 'page' => '13');
		$this->assertEqual($expected, $result->params);
	}

	/**
	 * Tests that routing is fully reset when calling `Router::reset()`.
	 */
	public function testResettingRoutes() {
		Router::connect('/{:controller}', array('controller' => 'posts'));
		$this->request->url = '/hello';

		$expected = array('controller' => 'hello', 'action' => 'index');
		$result = Router::parse($this->request);
		$this->assertEqual($expected, $result->params);

		Router::reset();
		$this->assertNull(Router::parse($this->request));
	}

	/**
	 * Tests matching routes where the route template is a static string with no insert parameters.
	 */
	public function testRouteMatchingWithNoInserts() {
		Router::connect('/login', array('controller' => 'sessions', 'action' => 'add'));
		$result = Router::match(array('controller' => 'sessions', 'action' => 'add'));
		$this->assertEqual('/login', $result);

		$this->expectException(
			"No parameter match found for URL `('controller' => 'Sessions', 'action' => 'index')`."
		);
		Router::match(array('controller' => 'sessions', 'action' => 'index'));
	}

	/**
	 * Test matching routes with only insert parameters and no default values.
	 */
	public function testRouteMatchingWithOnlyInserts() {
		Router::connect('/{:controller}');
		$this->assertEqual('/posts', Router::match(array('controller' => 'posts')));

		$this->expectException(
			"No parameter match found for URL `('controller' => 'Posts', 'action' => 'view')`."
		);
		Router::match(array('controller' => 'posts', 'action' => 'view'));
	}

	/**
	 * Test matching routes with insert parameters which have default values.
	 */
	public function testRouteMatchingWithInsertsAndDefaults() {
		Router::connect('/{:controller}/{:action}', array('action' => 'archive'));
		$this->assertEqual('/posts/index', Router::match(array('controller' => 'posts')));

		$result = Router::match(array('controller' => 'posts', 'action' => 'archive'));
		$this->assertEqual('/posts', $result);

		Router::reset();
		Router::connect('/{:controller}/{:action}', array('controller' => 'users'));

		$result = Router::match(array('action' => 'view'));
		$this->assertEqual('/users/view', $result);

		$result = Router::match(array('controller' => 'posts', 'action' => 'view'));
		$this->assertEqual('/posts/view', $result);

		$ex = "No parameter match found for URL ";
		$ex .= "`('controller' => 'Posts', 'action' => 'view', 'id' => '2')`.";
		$this->expectException($ex);
		Router::match(array('controller' => 'posts', 'action' => 'view', 'id' => '2'));
	}

	/**
	 * Tests matching routes and returning an absolute (protocol + hostname) URL.
	 */
	public function testRouteMatchAbsoluteUrl() {
		Router::connect('/login', array('controller' => 'sessions', 'action' => 'add'));
		$result = Router::match('Sessions::add', $this->request);
		$base = $this->request->env('base');
		$this->assertEqual($base . '/login', $result);

		$result = Router::match('Sessions::add', $this->request, array('absolute' => true));
		$base  = $this->request->env('HTTPS') ? 'https://' : 'http://';
		$base .= $this->request->env('HTTP_HOST');
		$base .= $this->request->env('base');
		$this->assertEqual($base . '/login', $result);

		$result = Router::match('Sessions::add',
			$this->request, array('host' => 'test.local', 'absolute' => true)
		);
		$base = $this->request->env('HTTPS') ? 'https://' : 'http://';
		$base .= 'test.local';
		$base .= $this->request->env('base');
		$this->assertEqual($base . '/login', $result);

		$result = Router::match('Sessions::add',
			$this->request, array('scheme' => 'https://', 'absolute' => true)
		);
		$base = 'https://' . $this->request->env('HTTP_HOST');
		$base .= $this->request->env('base');
		$this->assertEqual($base . '/login', $result);

		$result = Router::match('Sessions::add',
			$this->request, array('scheme' => 'https://', 'absolute' => true)
		);
		$base = 'https://' . $this->request->env('HTTP_HOST');
		$base .= $this->request->env('base');
		$this->assertEqual($base . '/login', $result);
	}

	/**
	 * Tests getting routes using `Router::get()`, and checking to see if the routes returned match
	 * the routes connected.
	 */
	public function testRouteRetrieval() {
		$expected = Router::connect('/hello', array('controller' => 'posts', 'action' => 'index'));
		$result = Router::get(0);
		$this->assertIdentical($expected, $result);

		list($result) = Router::get();
		$this->assertIdentical($expected, $result);
	}

	public function testStringUrlGeneration() {
		$result = Router::match('/posts');
		$expected = '/posts';
		$this->assertEqual($expected, $result);

		$result = Router::match('/posts');
		$this->assertEqual($expected, $result);

		$result = Router::match('/posts/view/5');
		$expected = '/posts/view/5';
		$this->assertEqual($expected, $result);

		$request = new Request(array('base' => '/my/web/path'));
		$result = Router::match('/posts', $request);
		$expected = '/my/web/path/posts';
		$this->assertEqual($expected, $result);

		$request = new Request(array('base' => '/my/web/path'));
		$result = Router::match('/some/where', $request, array('absolute' => true));
		$prefix  = $this->request->env('HTTPS') ? 'https://' : 'http://';
		$prefix .= $this->request->env('HTTP_HOST');
		$this->assertEqual($prefix . '/my/web/path/some/where', $result);


		$result = Router::match('mailto:foo@localhost');
		$expected = 'mailto:foo@localhost';
		$this->assertEqual($expected, $result);

		$result = Router::match('#top');
		$expected = '#top';
		$this->assertEqual($expected, $result);
	}

	public function testWithWildcardString() {
		Router::connect('/add/{:args}', array('controller' => 'tests', 'action' => 'add'));

		$expected = '/add';
		$result = Router::match('/add');
		$this->assertEqual($expected, $result);

		$expected = '/add/alke';
		$result = Router::match('/add/alke');
		$this->assertEqual($expected, $result);
	}

	public function testWithWildcardArray() {
		Router::connect('/add/{:args}', array('controller' => 'tests', 'action' => 'add'));

		$expected = '/add';
		$result = Router::match(array('controller' => 'tests', 'action' => 'add'));
		$this->assertEqual($expected, $result);

		$expected = '/add/alke';
		$result = Router::match(array(
			'controller' => 'tests', 'action' => 'add', 'args' => array('alke')
		));
		$this->assertEqual($expected, $result);

		$expected = '/add/alke/php';
		$result = Router::match(array(
			'controller' => 'tests', 'action' => 'add', 'args' => array('alke', 'php')
		));
		$this->assertEqual($expected, $result);
	}

	public function testProcess() {
		Router::connect('/add/{:args}', array('controller' => 'tests', 'action' => 'add'));
		$request = Router::process(new Request(array('url' => '/add/foo/bar')));

		$params = array('controller' => 'Tests', 'action' => 'add', 'args' => array('foo', 'bar'));
		$this->assertEqual($params, $request->params);
		$this->assertEqual(array('controller'), $request->persist);

		$request = Router::process(new Request(array('url' => '/remove/foo/bar')));
		$this->assertFalse($request->params);
	}

	/**
	 * Tests that the order of the parameters is respected so it can trim
	 * the URL correctly.
	 */
	public function testParameterOrderIsRespected() {
		Router::connect('/{:locale}/{:controller}/{:action}/{:args}');
		Router::connect('/{:controller}/{:action}/{:args}');

		$request = Router::process(new Request(array('url' => 'posts')));

		$url = Router::match('Posts::index', $request);
		$this->assertEqual($this->request->env('base') . '/posts', $url);

		$request = Router::process(new Request(array('url' => 'fr/posts')));

		$params = array('Posts::index', 'locale' => 'fr');
		$url = Router::match($params, $request);
		$this->assertEqual($this->request->env('base') . '/fr/posts', $url);
	}

	/**
	 * Tests that a request context with persistent parameters generates URLs where those parameters
	 * are properly taken into account.
	 */
	public function testParameterPersistence() {
		Router::connect('/{:controller}/{:action}/{:id:[0-9]+}', array(), array(
			'persist' => array('controller', 'id')
		));

		// URLs generated with $request will now have the 'controller' and 'id'
		// parameters copied to new URLs.
		$request = Router::process(new Request(array('url' => 'posts/view/1138')));

		$params = array('action' => 'edit');
		$url = Router::match($params, $request); // Returns: '/posts/edit/1138'
		$this->assertEqual($this->request->env('base') . '/posts/edit/1138', $url);

		Router::connect(
			'/add/{:args}',
			array('controller' => 'tests', 'action' => 'add'),
			array('persist' => array('controller', 'action'))
		);
		$request = Router::process(new Request(array('url' => '/add/foo/bar', 'base' => '')));
		$path = Router::match(array('args' => array('baz', 'dib')), $request);
		$this->assertEqual('/add/baz/dib', $path);
	}

	/**
	 * Tests that persistent parameters can be overridden with nulled-out values.
	 */
	public function testOverridingPersistentParameters() {
		Router::connect(
			'/admin/{:controller}/{:action}',
			array('admin' => true),
			array('persist' => array('admin', 'controller'))
		);
		Router::connect('/{:controller}/{:action}');

		$request = Router::process(new Request(array('url' => '/admin/posts/add', 'base' => '')));
		$expected = array('controller' => 'posts', 'action' => 'add', 'admin' => true);
		$this->assertEqual($expected, $request->params);
		$this->assertEqual(array('admin', 'controller'), $request->persist);

		$url = Router::match(array('action' => 'archive'), $request);
		$this->assertEqual('/admin/posts/archive', $url);

		$url = Router::match(array('action' => 'archive', 'admin' => null), $request);
		$this->assertEqual('/posts/archive', $url);
	}

	/**
	 * Tests passing a closure handler to `Router::connect()` to bypass or augment default
	 * dispatching.
	 */
	public function testRouteHandler() {
		Router::connect('/login', 'Users::login');

		Router::connect('/users/login', array(), function($request) {
			return new Response(array(
				'location' => array('controller' => 'users', 'action' => 'login')
			));
		});

		$result = Router::process(new Request(array('url' => '/users/login')));
		$this->assertTrue($result instanceof Response);

		$headers = array('Location' => '/login');
		$this->assertEqual($headers, $result->headers);
	}

	/**
	 * Tests that a successful match against a route with template `'/'` operating at the root of
	 * a domain never returns an empty string.
	 */
	public function testMatchingEmptyRoute() {
		Router::connect('/', 'Users::view');

		$request = new Request(array('base' => '/'));
		$url = Router::match(array('controller' => 'users', 'action' => 'view'), $request);
		$this->assertEqual('/', $url);

		$request = new Request(array('base' => ''));
		$url = Router::match(array('controller' => 'users', 'action' => 'view'), $request);
		$this->assertEqual('/', $url);
	}

	/**
	 * Tests routing based on content type extensions, with HTML being the default when types are
	 * not defined.
	 */
	public function testTypeBasedRouting() {
		Router::connect('/{:controller}/{:id:[0-9]+}', array(
			'action' => 'index', 'type' => 'html', 'id' => null
		));
		Router::connect('/{:controller}/{:id:[0-9]+}.{:type}', array(
			'action' => 'index', 'id' => null
		));

		Router::connect('/{:controller}/{:action}/{:id:[0-9]+}', array(
			'type' => 'html', 'id' => null
		));
		Router::connect('/{:controller}/{:action}/{:id:[0-9]+}.{:type}', array('id' => null));

		$url = Router::match(array('controller' => 'posts', 'type' => 'html'));
		$this->assertEqual('/posts', $url);

		$url = Router::match(array('controller' => 'posts', 'type' => 'json'));
		$this->assertEqual('/posts.json', $url);
	}

	/**
	 * Tests that routes can be connected and correctly match based on HTTP headers or method verbs.
	 */
	public function testHttpMethodBasedRouting() {
		Router::connect('/{:controller}/{:id:[0-9]+}', array(
			'http:method' => 'GET', 'action' => 'view'
		));
		Router::connect('/{:controller}/{:id:[0-9]+}', array(
			'http:method' => 'PUT', 'action' => 'edit'
		));

		$request = new Request(array('url' => '/posts/13', 'env' => array(
			'REQUEST_METHOD' => 'GET'
		)));
		$params = Router::process($request)->params;
		$expected = array('controller' => 'posts', 'action' => 'view', 'id' => '13');
		$this->assertEqual($expected, $params);

		$this->assertEqual('/posts/13', Router::match($params));

		$request = new Request(array('url' => '/posts/13', 'env' => array(
			'REQUEST_METHOD' => 'PUT'
		)));
		$params = Router::process($request)->params;
		$expected = array('controller' => 'posts', 'action' => 'edit', 'id' => '13');
		$this->assertEqual($expected, $params);

		$request = new Request(array('url' => '/posts/13', 'env' => array(
			'REQUEST_METHOD' => 'POST'
		)));
		$params = Router::process($request)->params;
		$this->assertFalse($params);
	}

	/**
	 * Tests that the class dependency configuration can be modified.
	 */
	public function testCustomConfiguration() {
		$old = Router::config();
		$config = array('classes' => array('route' => 'my\custom\Route'), 'unicode' => true);

		Router::config($config);
		$this->assertEqual($config, Router::config());

		Router::config($old);
		$this->assertEqual($old, Router::config());
	}

	/**
	 * Tests that continuation routes properly fall through and aggregate multiple route parameters.
	 */
	public function testRouteContinuations() {
		Router::connect('/{:locale:en|de|it|jp}/{:args}', array(), array('continue' => true));
		Router::connect('/{:controller}/{:action}/{:id:[0-9]+}');

		$request = new Request(array('url' => '/en/posts/view/1138'));
		$result = Router::process($request)->params;
		$expected = array (
			'controller' => 'posts', 'action' => 'view', 'id' => '1138', 'locale' => 'en'
		);
		$this->assertEqual($expected, $result);

		$request = new Request(array('url' => '/en/foo/bar/baz'));
		$this->assertNull(Router::parse($request));

		Router::reset();
		Router::connect('/{:args}/{:locale:en|de|it|jp}', array(), array('continue' => true));
		Router::connect('/{:controller}/{:action}/{:id:[0-9]+}');

		$request = new Request(array('url' => '/posts/view/1138/en'));
		$result = Router::process($request)->params;
		$this->assertEqual($expected, $result);

		Router::reset();
		Router::connect('/{:locale:en|de|it|jp}/{:args}', array(), array('continue' => true));
		Router::connect('/', 'Pages::view');

		$request = new Request(array('url' => '/en'));
		$result = Router::process($request)->params;
		$expected = array('locale' => 'en', 'controller' => 'Pages', 'action' => 'view');
		$this->assertEqual($expected, $result);
	}

	/**
	 * Tests that URLs are properly generated with route continuations.
	 */
	public function testReversingContinuations() {
		Router::connect('/{:locale:en|de|it|jp}/{:args}', array(), array('continue' => true));
		Router::connect('/{:controller}/{:action}/{:id:[0-9]+}');
		Router::connect('/{:controller}/{:action}/{:args}');

		$result = Router::match(array('Posts::view', 'id' => 5, 'locale' => 'de'));
		$this->assertEqual($result, '/de/posts/view/5');

		$result = Router::match(array('Posts::index', 'locale' => 'en', '?' => array('page' => 2)));
		$this->assertEqual('/en/posts?page=2', $result);

		Router::reset();
		Router::connect('/{:locale:en|de|it|jp}/{:args}', array(), array('continue' => true));
		Router::connect('/pages/{:args}', 'Pages::view');

		$result = Router::match(array('Pages::view', 'locale' => 'en', 'args' => array('about')));
		$this->assertEqual('/en/pages/about', $result);

		Router::reset();
		Router::connect('/admin/{:args}', array('admin' => true), array('continue' => true));
		Router::connect('/login', 'Users::login');

		$result = Router::match(array('Users::login', 'admin' => true));
		$this->assertEqual('/admin/login', $result);
	}

	/**
	 * Tests that multiple continuation routes can be applied to the same URL.
	 */
	public function testStackedContinuationRoutes() {
		Router::connect('/admin/{:args}', array('admin' => true), array('continue' => true));
		Router::connect('/{:locale:en|de|it|jp}/{:args}', array(), array('continue' => true));
		Router::connect('/{:controller}/{:action}/{:id:[0-9]+}', array('id' => null));

		$request = new Request(array('url' => '/en/foo/bar/5'));
		$expected = array('controller' => 'foo', 'action' => 'bar', 'id' => '5', 'locale' => 'en');
		$this->assertEqual($expected, Router::process($request)->params);

		$request = new Request(array('url' => '/admin/foo/bar/5'));
		$expected = array('controller' => 'foo', 'action' => 'bar', 'id' => '5', 'admin' => true);
		$this->assertEqual($expected, Router::process($request)->params);

		$request = new Request(array('url' => '/admin/de/foo/bar/5'));
		$expected = array(
			'controller' => 'foo', 'action' => 'bar', 'id' => '5', 'locale' => 'de', 'admin' => true
		);
		$this->assertEqual($expected, Router::process($request)->params);

		$request = new Request(array('url' => '/en/admin/foo/bar/5'));
		$this->assertFalse(Router::process($request)->params);

		$result = Router::match(array('Foo::bar', 'id' => 5));
		$this->assertEqual('/foo/bar/5', $result);

		$result = Router::match(array('Foo::bar', 'id' => 5, 'admin' => true));
		$this->assertEqual('/admin/foo/bar/5', $result);

		$result = Router::match(array('Foo::bar', 'id' => 5, 'admin' => true, 'locale' => 'jp'));
		$this->assertEqual('/admin/jp/foo/bar/5', $result);
	}

	/**
	 * Tests that continuations can be used for route suffixes.
	 */
	public function testSuffixContinuation() {
		Router::connect("/{:args}.{:type}", array(), array('continue' => true));
		Router::connect('/{:controller}/{:id:[0-9]+}', array('action' => 'view'));

		$result = Router::match(array(
			'controller' => 'versions',
			'action' => 'view',
			'id' => 13,
			'type' => 'jsonp'
		));
		$this->assertEqual('/versions/13.jsonp', $result);

		$result = Router::match(array(
			'controller' => 'versions',
			'action' => 'view',
			'id' => 13
		));
		$this->assertEqual('/versions/13', $result);
	}

	/**
	 * Tests default route formatters, and setting/getting new formatters.
	 */
	public function testRouteFormatters() {
		$formatters = Router::formatters();
		$this->assertEqual(array('args', 'controller'), array_keys($formatters));

		$this->assertEqual('foo/bar', $formatters['args'](array('foo', 'bar')));
		$this->assertEqual('list_items', $formatters['controller']('ListItems'));

		Router::formatters(array('action' => function($value) { return strtolower($value); }));
		$formatters = Router::formatters();
		$this->assertEqual(array('action', 'args', 'controller'), array_keys($formatters));

		Router::formatters(array('action' => null));
		$formatters = Router::formatters();
		$this->assertEqual(array('args', 'controller'), array_keys($formatters));
	}
}

?>