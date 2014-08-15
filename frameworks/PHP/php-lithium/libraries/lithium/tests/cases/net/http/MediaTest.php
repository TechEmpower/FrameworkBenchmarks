<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\net\http;

use lithium\core\Environment;
use lithium\net\http\Media;
use lithium\action\Request;
use lithium\action\Response;
use lithium\core\Libraries;
use lithium\data\entity\Record;
use lithium\data\collection\RecordSet;

class MediaTest extends \lithium\test\Unit {

	/**
	 * Reset the `Media` class to its default state.
	 */
	public function setUp() {
		Media::reset();
	}

	/**
	 * Tests setting, getting and removing custom media types.
	 */
	public function testMediaTypes() {
		// Get a list of all available media types:
		$types = Media::types(); // returns array('html', 'json', 'rss', ...);

		$expected = array(
			'html', 'htm', 'form', 'json', 'rss', 'atom', 'css', 'js', 'text', 'txt', 'xml'
		);
		$this->assertEqual($expected, $types);
		$this->assertEqual($expected, Media::formats());

		$result = Media::type('json');
		$expected = array('application/json');
		$this->assertEqual($expected, $result['content']);

		$expected = array(
			'cast' => true, 'encode' => 'json_encode', 'decode' => $result['options']['decode']
		);
		$this->assertEqual($expected, $result['options']);

		// Add a custom media type with a custom view class:
		Media::type('my', 'text/x-my', array(
			'view' => 'my\custom\View',
			'paths' => array('layout' => false)
		));

		$result = Media::types();
		$this->assertTrue(in_array('my', $result));

		$result = Media::type('my');
		$expected = array('text/x-my');
		$this->assertEqual($expected, $result['content']);

		$expected = array(
			'view' => 'my\custom\View',
			'paths' => array(
				'template' => '{:library}/views/{:controller}/{:template}.{:type}.php',
				'layout' => false,
				'element' => '{:library}/views/elements/{:template}.{:type}.php'
			),
			'encode' => null, 'decode' => null, 'cast' => true, 'conditions' => array()
		);
		$this->assertEqual($expected, $result['options']);

		// Remove a custom media type:
		Media::type('my', false);
		$result = Media::types();
		$this->assertFalse(in_array('my', $result));
	}

	/**
	 * Tests that `Media` will return the correct type name of recognized, registered content types.
	 */
	public function testContentTypeDetection() {
		$this->assertNull(Media::type('application/foo'));
		$this->assertEqual('js', Media::type('application/javascript'));
		$this->assertEqual('html', Media::type('*/*'));
		$this->assertEqual('json', Media::type('application/json'));
		$this->assertEqual('json', Media::type('application/json; charset=UTF-8'));

		$result = Media::type('json');
		$expected = array('content' => array('application/json'), 'options' => array(
			'cast' => true, 'encode' => 'json_encode', 'decode' => $result['options']['decode']
		));
		$this->assertEqual($expected, $result);
	}

	public function testAssetTypeHandling() {
		$result = Media::assets();
		$expected = array('js', 'css', 'image', 'generic');
		$this->assertEqual($expected, array_keys($result));

		$result = Media::assets('css');
		$expected = '.css';
		$this->assertEqual($expected, $result['suffix']);
		$this->assertTrue(isset($result['path']['{:base}/{:library}/css/{:path}']));

		$result = Media::assets('my');
		$this->assertNull($result);

		$result = Media::assets('my', array('suffix' => '.my', 'path' => array(
			'{:base}/my/{:path}' => array('base', 'path')
		)));
		$this->assertNull($result);

		$result = Media::assets('my');
		$expected = '.my';
		$this->assertEqual($expected, $result['suffix']);
		$this->assertTrue(isset($result['path']['{:base}/my/{:path}']));

		$this->assertNull($result['filter']);
		Media::assets('my', array('filter' => array('/my/' => '/your/')));

		$result = Media::assets('my');
		$expected = array('/my/' => '/your/');
		$this->assertEqual($expected, $result['filter']);

		$expected = '.my';
		$this->assertEqual($expected, $result['suffix']);

		Media::assets('my', false);
		$result = Media::assets('my');
		$this->assertNull($result);

		$this->assertEqual('/foo.exe', Media::asset('foo.exe', 'bar'));
	}

	public function testAssetAbsoluteRelativePaths() {
		$result = Media::asset('scheme://host/subpath/file', 'js');
		$expected = 'scheme://host/subpath/file';
		$this->assertEqual($expected, $result);

		$result = Media::asset('//host/subpath/file', 'js', array('base' => '/base'));
		$expected = '//host/subpath/file';
		$this->assertEqual($expected, $result);

		$result = Media::asset('subpath/file', 'js');
		$expected = '/js/subpath/file.js';
		$this->assertEqual($expected, $result);
	}

	public function testCustomAssetUrls() {
		$env = Environment::get();

		$path = Libraries::get(true, 'path');
		Libraries::add('cdn_js_test', array(
			'path' => $path,
			'assets' => array(
				'js' => 'http://static.cdn.com'
			),
			'bootstrap' => false
		));

		Libraries::add('cdn_env_test', array(
			'path' => $path,
			'assets' => array(
				'js' => 'wrong',
				$env => array('js' => 'http://static.cdn.com/myapp')
			),
			'bootstrap' => false
		));
		$library = basename($path);

		$result = Media::asset('foo', 'js', array('library' => 'cdn_js_test'));
		$this->assertEqual("http://static.cdn.com/{$library}/js/foo.js", $result);

		$result = Media::asset('foo', 'css', array('library' => 'cdn_js_test'));
		$this->assertEqual("/{$library}/css/foo.css", $result);

		$result = Media::asset('foo', 'js', array('library' => 'cdn_env_test'));
		$this->assertEqual("http://static.cdn.com/myapp/{$library}/js/foo.js", $result);

		Libraries::remove('cdn_env_test');
		Libraries::remove('cdn_js_test');
	}

	public function testAssetPathGeneration() {
		$resources = Libraries::get(true, 'resources');
		$this->skipIf(!is_writable($resources), "Cannot write test app to resources directory.");
		$paths = array("{$resources}/media_test/webroot/css", "{$resources}/media_test/webroot/js");

		foreach ($paths as $path) {
			if (!is_dir($path)) {
				mkdir($path, 0777, true);
			}
		}
		touch("{$paths[0]}/debug.css");

		Libraries::add('media_test', array('path' => "{$resources}/media_test"));

		$result = Media::asset('debug', 'css', array('check' => true, 'library' => 'media_test'));
		$this->assertEqual('/media_test/css/debug.css', $result);

		$result = Media::asset('debug', 'css', array(
			'timestamp' => true, 'library' => 'media_test'
		));
		$this->assertPattern('%^/media_test/css/debug\.css\?\d+$%', $result);

		$result = Media::asset('debug.css?type=test', 'css', array(
			'check' => true, 'base' => 'foo', 'library' => 'media_test'
		));
		$this->assertEqual('foo/media_test/css/debug.css?type=test', $result);

		$result = Media::asset('debug.css?type=test', 'css', array(
			'check' => true, 'base' => 'foo', 'timestamp' => true, 'library' => 'media_test'
		));
		$this->assertPattern('%^foo/media_test/css/debug\.css\?type=test&\d+$%', $result);

		$file = Media::path('css/debug.css', 'bar', array('library' => 'media_test'));
		$this->assertTrue(file_exists($file));

		$result = Media::asset('this.file.should.not.exist', 'css', array('check' => true));
		$this->assertFalse($result);

		unlink("{$paths[0]}/debug.css");

		foreach (array_merge($paths, array(dirname($paths[0]))) as $path) {
			rmdir($path);
		}
	}

	public function testCustomAssetPathGeneration() {
		Media::assets('my', array('suffix' => '.my', 'path' => array(
			'{:base}/my/{:path}' => array('base', 'path')
		)));

		$result = Media::asset('subpath/file', 'my');
		$expected = '/my/subpath/file.my';
		$this->assertEqual($expected, $result);

		Media::assets('my', array('filter' => array('/my/' => '/your/')));

		$result = Media::asset('subpath/file', 'my');
		$expected = '/your/subpath/file.my';
		$this->assertEqual($expected, $result);

		$result = Media::asset('subpath/file', 'my', array('base' => '/app/path'));
		$expected = '/app/path/your/subpath/file.my';
		$this->assertEqual($expected, $result);

		$result = Media::asset('subpath/file', 'my', array('base' => '/app/path/'));
		$expected = '/app/path//your/subpath/file.my';
		$this->assertEqual($expected, $result);
	}

	public function testMultiLibraryAssetPaths() {
		$result = Media::asset('path/file', 'js', array('library' => true, 'base' => '/app/base'));
		$expected = '/app/base/js/path/file.js';
		$this->assertEqual($expected, $result);

		Libraries::add('li3_foo_blog', array(
			'path' => LITHIUM_APP_PATH . '/libraries/plugins/blog',
			'bootstrap' => false,
			'route' => false
		));

		$result = Media::asset('path/file', 'js', array(
			'library' => 'li3_foo_blog', 'base' => '/app/base'
		));
		$expected = '/app/base/blog/js/path/file.js';
		$this->assertEqual($expected, $result);

		Libraries::remove('li3_foo_blog');
	}

	public function testManualAssetPaths() {
		$result = Media::asset('/path/file', 'js', array('base' => '/base'));
		$expected = '/base/path/file.js';
		$this->assertEqual($expected, $result);

		$resources = Libraries::get(true, 'resources');
		$cssPath = "{$resources}/media_test/webroot/css";
		$this->skipIf(!is_writable($resources), "Cannot write test app to resources directory.");

		if (!is_dir($cssPath)) {
			mkdir($cssPath, 0777, true);
		}

		Libraries::add('media_test', array('path' => "{$resources}/media_test"));

		$result = Media::asset('/foo/bar', 'js', array('base' => '/base', 'check' => true));
		$this->assertFalse($result);

		file_put_contents("{$cssPath}/debug.css", "html, body { background-color: black; }");
		$result = Media::asset('/css/debug', 'css', array(
			'library' => 'media_test', 'base' => '/base', 'check' => true
		));
		$expected = '/base/css/debug.css';
		$this->assertEqual($expected, $result);

		$result = Media::asset('/css/debug.css', 'css', array(
			'library' => 'media_test', 'base' => '/base', 'check' => true
		));
		$expected = '/base/css/debug.css';
		$this->assertEqual($expected, $result);

		$result = Media::asset('/css/debug.css?foo', 'css', array(
			'library' => 'media_test', 'base' => '/base', 'check' => true
		));
		$expected = '/base/css/debug.css?foo';
		$this->assertEqual($expected, $result);

		Libraries::remove('media_test');
		unlink("{$cssPath}/debug.css");

		foreach (array($cssPath, dirname($cssPath)) as $path) {
			rmdir($path);
		}
	}

	public function testRender() {
		$response = new Response();
		$response->type('json');
		$data = array('something');
		Media::render($response, $data);

		$result = $response->headers();
		$this->assertEqual(array('Content-Type: application/json; charset=UTF-8'), $result);

		$result = $response->body();
		$this->assertEqual($data, $result);
	}

	/**
	 * Tests that a decode handler is not called when the Media type has none configured.
	 */
	public function testNoDecode() {
		Media::type('my', 'text/x-my', array('decode' => false));

		$result = Media::decode('my', 'Hello World');
		$this->assertEqual(null, $result);
	}

	/**
	 * Tests that types with decode handlers can properly decode content.
	 */
	public function testDecode() {
		$data = array('movies' => array(
			array('name' => 'Shaun of the Dead', 'year' => 2004),
			array('name' => 'V for Vendetta', 'year' => 2005)
		));
		$jsonEncoded = '{"movies":[{"name":"Shaun of the Dead","year":2004},';
		$jsonEncoded .= '{"name":"V for Vendetta","year":2005}]}';

		$result = Media::decode('json', $jsonEncoded);
		$this->assertEqual($data, $result);

		$formEncoded = 'movies%5B0%5D%5Bname%5D=Shaun+of+the+Dead&movies%5B0%5D%5Byear%5D=2004';
		$formEncoded .= '&movies%5B1%5D%5Bname%5D=V+for+Vendetta&movies%5B1%5D%5Byear%5D=2005';

		$result = Media::decode('form', $formEncoded);
		$this->assertEqual($data, $result);
	}

	public function testCustomEncodeHandler() {
		$response = new Response();

		Media::type('csv', 'application/csv', array(
			'encode' => function($data) {
				ob_start();
				$out = fopen('php://output', 'w');
				foreach ($data as $record) {
					fputcsv($out, $record);
				}
				fclose($out);
				return ob_get_clean();
			}
		));

		$data = array(
			array('John', 'Doe', '123 Main St.', 'Anytown, CA', '91724'),
			array('Jane', 'Doe', '124 Main St.', 'Anytown, CA', '91724')
		);
		$response->type('csv');
		Media::render($response, $data);
		$result = $response->body;
		$expected = 'John,Doe,"123 Main St.","Anytown, CA",91724' . "\n";
		$expected .= 'Jane,Doe,"124 Main St.","Anytown, CA",91724' . "\n";
		$this->assertEqual(array($expected), $result);

		$result = $response->headers['Content-Type'];
		$this->assertEqual('application/csv; charset=UTF-8', $result);
	}

	public function testEmptyEncode() {
		$handler = Media::type('empty', 'empty/encode');
		$this->assertNull(Media::encode($handler, array()));

		$handler = Media::type('empty', 'empty/encode', array(
			'encode' => null
		));
		$this->assertNull(Media::encode($handler, array()));

		$handler = Media::type('empty', 'empty/encode', array(
			'encode' => false
		));
		$this->assertNull(Media::encode($handler, array()));

		$handler = Media::type('empty', 'empty/encode', array(
			'encode' => ""
		));
		$this->assertNull(Media::encode($handler, array()));
	}

	/**
	 * Tests that rendering plain text correctly returns the render data as-is.
	 *
	 * @return void
	 */
	public function testPlainTextOutput() {
		$response = new Response();
		$response->type('text');
		Media::render($response, "Hello, world!");

		$result = $response->body;
		$this->assertEqual(array("Hello, world!"), $result);
	}

	/**
	 * Tests that an exception is thrown for cases where an attempt is made to render content for
	 * a type which is not registered.
	 *
	 * @return void
	 */
	public function testUndhandledContent() {
		$response = new Response();
		$response->type('bad');

		$this->expectException("Unhandled media type `bad`.");
		Media::render($response, array('foo' => 'bar'));

		$result = $response->body();
		$this->assertNull($result);
	}

	/**
	 * Tests that attempts to render a media type with no handler registered produces an
	 * 'unhandled media type' exception, even if the type itself is a registered content type.
	 *
	 * @return void
	 */
	public function testUnregisteredContentHandler() {
		$response = new Response();
		$response->type('xml');

		$this->expectException("Unhandled media type `xml`.");
		Media::render($response, array('foo' => 'bar'));

		$result = $response->body;
		$this->assertNull($result);
	}

	/**
	 * Tests handling content type manually using parameters to `Media::render()`, for content types
	 * that are registered but have no default handler.
	 *
	 * @return void
	 */
	public function testManualContentHandling() {
		Media::type('custom', 'text/x-custom');
		$response = new Response();
		$response->type('custom');

		Media::render($response, 'Hello, world!', array(
			'layout' => false,
			'template' => false,
			'encode' => function($data) { return "Message: {$data}"; }
		));

		$result = $response->body;
		$expected = array("Message: Hello, world!");
		$this->assertEqual($expected, $result);

		$this->expectException("/Template not found/");
		Media::render($response, 'Hello, world!');

		$result = $response->body;
		$this->assertNull($result);
	}

	/**
	 * Tests that parameters from the `Request` object passed into `render()` via
	 * `$options['request']` are properly merged into the `$options` array passed to render
	 * handlers.
	 *
	 * @return void
	 */
	public function testRequestOptionMerging() {
		Media::type('custom', 'text/x-custom');
		$request = new Request();
		$request->params['foo'] = 'bar';

		$response = new Response();
		$response->type('custom');

		Media::render($response, null, compact('request') + array(
			'layout' => false,
			'template' => false,
			'encode' => function($data, $handler) { return $handler['request']->foo; }
		));
		$this->assertEqual(array('bar'), $response->body);
	}

	public function testMediaEncoding() {
		$data = array('hello', 'goodbye', 'foo' => array('bar', 'baz' => 'dib'));
		$expected = json_encode($data);
		$result = Media::encode('json', $data);
		$this->assertEqual($expected, $result);

		$this->assertEqual($result, Media::to('json', $data));
		$this->assertNull(Media::encode('badness', $data));

		$result = Media::decode('json', $expected);
		$this->assertEqual($data, $result);
	}

	public function testRenderWithOptionsMerging() {
		$base = Libraries::get(true, 'resources') . '/tmp';
		$this->skipIf(!is_writable($base), "Path `{$base}` is not writable.");

		$request = new Request();
		$request->params['controller'] = 'pages';

		$response = new Response();
		$response->type('html');

		$this->expectException('/Template not found/');
		Media::render($response, null, compact('request'));
		$this->_cleanUp();
	}

	public function testCustomWebroot() {
		Libraries::add('defaultStyleApp', array('path' => LITHIUM_APP_PATH, 'bootstrap' => false));
		$this->assertEqual(
			realpath(LITHIUM_APP_PATH . '/webroot'),
			realpath(Media::webroot('defaultStyleApp'))
		);

		Libraries::add('customWebRootApp', array(
			'path' => LITHIUM_APP_PATH,
			'webroot' => LITHIUM_APP_PATH,
			'bootstrap' => false
		));
		$this->assertEqual(LITHIUM_APP_PATH, Media::webroot('customWebRootApp'));

		Libraries::remove('defaultStyleApp');
		Libraries::remove('customWebRootApp');
		$this->assertNull(Media::webroot('defaultStyleApp'));
	}

	/**
	 * Tests that the `Media` class' configuration can be reset to its default state.
	 *
	 * @return void
	 */
	public function testStateReset() {
		$this->assertFalse(in_array('foo', Media::types()));

		Media::type('foo', 'text/x-foo');
		$this->assertTrue(in_array('foo', Media::types()));

		Media::reset();
		$this->assertFalse(in_array('foo', Media::types()));
	}

	public function testEncodeRecordSet() {
		$data = new RecordSet(array('data' => array(
			1 => new Record(array('data' => array('id' => 1, 'foo' => 'bar'))),
			2 => new Record(array('data' => array('id' => 2, 'foo' => 'baz'))),
			3 => new Record(array('data' => array('id' => 3, 'baz' => 'dib')))
		)));
		$json = '{"1":{"id":1,"foo":"bar"},"2":{"id":2,"foo":"baz"},"3":{"id":3,"baz":"dib"}}';
		$this->assertEqual($json, Media::encode(array('encode' => 'json_encode'), $data));
	}

	public function testEncodeNotCallable() {
		$data = array('foo' => 'bar');
		$result = Media::encode(array('encode' => false), $data);
		$this->assertNull($result);
	}

	/**
	 * Tests that calling `Media::type()` to retrieve the details of a type that is aliased to
	 * another type, automatically resolves to the settings of the type being pointed at.
	 *
	 * @return void
	 */
	public function testTypeAliasResolution() {
		$resolved = Media::type('text');
		$this->assertEqual(array('text/plain'), $resolved['content']);
		unset($resolved['options']['encode']);

		$result = Media::type('txt');
		unset($result['options']['encode']);
		$this->assertEqual($resolved, $result);
	}

	public function testQueryUndefinedAssetTypes() {
		$base = Media::path('index.php', 'generic');
		$result = Media::path('index.php', 'foo');
		$this->assertEqual($result, $base);

		$base = Media::asset('/bar', 'generic');
		$result = Media::asset('/bar', 'foo');
		$this->assertEqual($result, $base);
	}

	public function testGetLibraryWebroot() {
		$this->assertNull(Media::webroot('foobar'));

		Libraries::add('foobar', array('path' => __DIR__, 'webroot' => __DIR__));
		$this->assertEqual(__DIR__, Media::webroot('foobar'));
		Libraries::remove('foobar');

		$resources = Libraries::get(true, 'resources');
		$webroot = "{$resources}/media_test/webroot";
		$this->skipIf(!is_writable($resources), "Cannot write test app to resources directory.");

		if (!is_dir($webroot)) {
			mkdir($webroot, 0777, true);
		}

		Libraries::add('media_test', array('path' => "{$resources}/media_test"));
		$this->assertTrue(is_dir(Media::webroot('media_test')));
		Libraries::remove('media_test');
		rmdir($webroot);
	}

	/**
	 * Tests that the `Response` object can be directly modified from a templating class or encode
	 * function.
	 *
	 * @return void
	 */
	public function testResponseModification() {
		Media::type('my', 'text/x-my', array('view' => 'lithium\tests\mocks\net\http\Template'));
		$response = new Response();

		Media::render($response, null, array('type' => 'my'));
		$this->assertEqual('Value', $response->headers('Custom'));
	}

	/**
	 * Tests that `Media::asset()` will not prepend path strings with the base application path if
	 * it has already been prepended.
	 *
	 * @return void
	 */
	public function testDuplicateBasePathCheck() {
		$result = Media::asset('/foo/bar/image.jpg', 'image', array('base' => '/bar'));
		$this->assertEqual('/bar/foo/bar/image.jpg', $result);

		$result = Media::asset('/foo/bar/image.jpg', 'image', array('base' => '/foo/bar'));
		$this->assertEqual('/foo/bar/image.jpg', $result);

		$result = Media::asset('foo/bar/image.jpg', 'image', array('base' => 'foo'));
		$this->assertEqual('foo/img/foo/bar/image.jpg', $result);

		$result = Media::asset('/foo/bar/image.jpg', 'image', array('base' => ''));
		$this->assertEqual('/foo/bar/image.jpg', $result);
	}

	public function testContentNegotiationByType() {
		$this->assertEqual('html', Media::type('text/html'));

		Media::type('jsonp', 'text/html', array(
			'conditions' => array('type' => true)
		));
		$this->assertEqual(array('jsonp', 'html'), Media::type('text/html'));

		$config = array('env' => array('HTTP_ACCEPT' => 'text/html,text/plain;q=0.5'));
		$request = new Request($config);
		$request->params = array('type' => 'jsonp');
		$this->assertEqual('jsonp', Media::negotiate($request));

		$request = new Request($config);
		$this->assertEqual('html', Media::negotiate($request));
	}

	public function testContentNegotiationByUserAgent() {
		Media::type('iphone', 'application/xhtml+xml', array(
			'conditions' => array('mobile' => true)
		));
		$request = new Request(array('env' => array(
			'HTTP_USER_AGENT' => 'Safari',
			'HTTP_ACCEPT' => 'application/xhtml+xml,text/html'
		)));
		$this->assertEqual('html', Media::negotiate($request));

		$request = new Request(array('env' => array(
			'HTTP_USER_AGENT' => 'iPhone',
			'HTTP_ACCEPT' => 'application/xhtml+xml,text/html'
		)));
		$this->assertEqual('iphone', Media::negotiate($request));
	}

	/**
	 * Tests that empty asset paths correctly return the base path for the asset type, and don't
	 * generate notices or errors.
	 */
	public function testEmptyAssetPaths() {
		$this->assertEqual('/img/', Media::asset('', 'image'));
		$this->assertEqual('/css/.css', Media::asset('', 'css'));
		$this->assertEqual('/js/.js', Media::asset('', 'js'));
		$this->assertEqual('/', Media::asset('', 'generic'));
	}
}

?>