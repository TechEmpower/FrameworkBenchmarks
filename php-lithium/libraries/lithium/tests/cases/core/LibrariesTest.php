<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright	 Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license	   http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\core;

use stdClass;
use SplFileInfo;
use lithium\util\Inflector;
use lithium\core\Libraries;
use lithium\template\view\adapter\Simple;

class LibrariesTest extends \lithium\test\Unit {

	protected $_cache = array();

	public function setUp() {
		$this->_cache = Libraries::cache();
		Libraries::cache(false);
		$this->hasApp = preg_match('/app$/', LITHIUM_APP_PATH);
	}

	public function tearDown() {
		Libraries::cache(false);
		Libraries::cache($this->_cache);
		unset($this->hasApp);
	}

	public function testNamespaceToFileTranslation() {
		$ds = DIRECTORY_SEPARATOR;
		$invalidDS = $ds == '/' ? '\\' : '/';

		$result = Libraries::path('\lithium\core\Libraries');
		$this->assertTrue(strpos($result, "${ds}lithium${ds}core${ds}Libraries.php"));
		$this->assertTrue(file_exists($result));
		$this->assertFalse(strpos($result, $invalidDS));

		$result = Libraries::path('lithium\core\Libraries');
		$this->assertTrue(strpos($result, "${ds}lithium${ds}core${ds}Libraries.php"));
		$this->assertTrue(file_exists($result));
		$this->assertFalse(strpos($result, $invalidDS));
	}

	public function testPathTemplate() {
		$expected = array('{:app}/libraries/{:name}', '{:root}/{:name}');
		$result = Libraries::paths('libraries');
		$this->assertEqual($expected, $result);

		$this->assertNull(Libraries::locate('authAdapter', 'Form'));

		$paths = Libraries::paths();
		$test = array('authAdapter' => array('lithium\security\auth\adapter\{:name}'));
		Libraries::paths($test);
		$this->assertEqual($paths + $test, Libraries::paths());

		$class = Libraries::locate('authAdapter', 'Form');
		$expected = 'lithium\security\auth\adapter\Form';
		$this->assertEqual($expected, $class);

		Libraries::paths($paths + array('authAdapter' => false));
		$this->assertEqual($paths, Libraries::paths());
	}

	public function testPathTemplateWithGlobBrace() {
		Libraries::paths(array(
			'analysis' => array(
				'{:library}\analysis\*{Docblock,Debugger}',
			),
		));

		$analysis = list($docblock, $debugger) = Libraries::locate('analysis', null, array(
			'recursive' => false,
			'format' => false,
		));

		$this->assertCount(2, $analysis);
		$this->assertPattern('/Docblock\.php/', $docblock);
		$this->assertPattern('/Debugger\.php/', $debugger);
	}

	public function testPathTransform() {
		$expected = 'Library/Class/Separated/By/Underscore';
		$result = Libraries::path('Library_Class_Separated_By_Underscore', array(
			'prefix' => 'Library_',
			'transform' => function ($class, $options) {
				return str_replace('_', '/', $class);
			}
		));
		$this->assertEqual($expected, $result);

		$expected = 'Library/Class/Separated/By/Nothing';
		$result = Libraries::path('LibraryClassSeparatedByNothing', array(
			'prefix' => 'Library',
			'transform' => array('/([a-z])([A-Z])/', '$1/$2')
		));
		$this->assertEqual($expected, $result);
	}

	public function testPathFiltering() {
		$tests = Libraries::find('lithium', array('recursive' => true, 'path' => '/tests/cases'));
		$result = preg_grep('/^lithium\\\\tests\\\\cases\\\\/', $tests);
		$this->assertIdentical($tests, $result);

		$all = Libraries::find('lithium', array('recursive' => true));
		$result = array_values(preg_grep('/^lithium\\\\tests\\\\cases\\\\/', $all));
		$this->assertIdentical($tests, $result);

		if ($this->hasApp) {
			$tests = Libraries::find('app', array('recursive' => true, 'path' => '/tests/cases'));
			$result = preg_grep('/^app\\\\tests\\\\cases\\\\/', $tests);
			$this->assertIdentical($tests, $result);
		}
	}

	/**
	 * Tests accessing library configurations.
	 */
	public function testLibraryConfigAccess() {
		$config = Libraries::get('lithium'); // => ['path' => '/path/to/lithium', ...]
		$expected = array(
			'path' => str_replace('\\', '/', realpath(realpath(LITHIUM_LIBRARY_PATH) . '/lithium')),
			'prefix' => 'lithium\\',
			'suffix' => '.php',
			'loader' => 'lithium\\core\\Libraries::load',
			'includePath' => false,
			'transform' => null,
			'bootstrap' => false,
			'defer' => true,
			'default' => false
		);

		if (!$this->hasApp) {
			$expected['resources'] = sys_get_temp_dir();
			$expected['default'] = true;
		}

		$this->assertEqual($expected, $config);
		$this->assertNull(Libraries::get('foo'));

		$configs = Libraries::get(); // => ['lithium' => ['path' => ...], 'myapp' => [...], ...]
		$this->assertTrue(isset($configs['lithium']));
		$this->assertEqual($expected, $configs['lithium']);

		if ($this->hasApp) {
			$this->assertTrue(isset($configs['app']));
		}

		$configs = Libraries::get(array('lithium')); // => ['lithium' => ['path' => '...', ...]]
		$this->assertEqual(array('lithium'), array_keys($configs));
		$this->assertEqual($expected, $configs['lithium']);

		$prefixes = Libraries::get(array('lithium'), 'prefix'); // => ['lithium' => 'lithium\\']
		$this->assertEqual(array('lithium' => 'lithium\\'), $prefixes);

		$allPre = Libraries::get(null, 'prefix'); // => ['my' => 'my\\', 'lithium' => 'lithium\\']
		$this->assertTrue($allPre);
		$this->assertEqual(array_keys(Libraries::get()), array_keys($allPre));

		foreach ($allPre as $prefix) {
			$this->assertTrue(is_string($prefix) || is_bool($prefix));
		}

		$library = Libraries::get('lithium\core\Libraries'); // 'lithium'
		$this->assertEqual('lithium', $library);
		$this->assertNull(Libraries::get('foo\bar\baz'));
	}

	/**
	 * Tests the addition and removal of default libraries.
	 */
	public function testLibraryAddRemove() {
		$lithium = Libraries::get('lithium');
		$this->assertFalse(empty($lithium));

		$app = Libraries::get(true);
		$this->assertFalse(empty($app));

		Libraries::remove(array('lithium', 'app'));

		$result = Libraries::get('lithium');
		$this->assertTrue(empty($result));

		$result = Libraries::get('app');
		$this->assertTrue(empty($result));

		$result = Libraries::add('lithium', array('bootstrap' => false) + $lithium);
		$this->assertEqual($lithium, $result);

		$result = Libraries::add('app', array('bootstrap' => false) + $app);
		$this->assertEqual(array('bootstrap' => false) + $app, $result);
	}

	/**
	* Tests that an exception is thrown when a library is added which could not be found.
	*/
	public function testAddInvalidLibrary() {
		$this->expectException("Library `invalid_foo` not found.");
		Libraries::add('invalid_foo');
	}

	/**
	 * Tests that non-prefixed (poorly named or structured) libraries can still be added.
	 */
	public function testAddNonPrefixedLibrary() {
		$tmpDir = realpath(Libraries::get(true, 'resources') . '/tmp');
		$this->skipIf(!is_writable($tmpDir), "Can't write to resources directory.");

		$fakeDir = $tmpDir . '/fake';
		$fake = "<?php class Fake {} ?>";
		$fakeFilename = $fakeDir . '/fake.php';
		mkdir($fakeDir, 0777, true);
		file_put_contents($fakeFilename, $fake);

		Libraries::add('bad', array(
			'prefix' => false,
			'path' => $fakeDir,
			'transform' => function($class, $config) { return ''; }
		));

		Libraries::add('fake', array(
			'path' => $fakeDir,
			'includePath' => true,
			'prefix' => false,
			'transform' => function($class, $config) {
				return $config['path'] . '/' . Inflector::underscore($class) . '.php';
			}
		));

		$this->assertFalse(class_exists('Fake', false));
		$this->assertTrue(class_exists('Fake'));
		unlink($fakeFilename);
		rmdir($fakeDir);
		Libraries::remove('fake');
	}

	/**
	 * Tests that non-class files are always filtered out of `find()` results unless an alternate
	 * filter is specified.
	 */
	public function testExcludeNonClassFiles() {
		$result = Libraries::find('lithium');
		$this->assertFalse($result);

		$result = Libraries::find('lithium', array('namespaces' => true));

		$this->assertTrue(in_array('lithium\action', $result));
		$this->assertTrue(in_array('lithium\core', $result));
		$this->assertTrue(in_array('lithium\util', $result));

		$this->assertFalse(in_array('lithium\LICENSE.txt', $result));
		$this->assertFalse(in_array('lithium\readme.wiki', $result));

		$this->assertFalse(Libraries::find('lithium'));
		$result = Libraries::find('lithium', array('path' => '/test/filter/reporter/template'));
		$this->assertFalse($result);

		$result = Libraries::find('lithium', array(
			'path' => '/test/filter/reporter/template',
			'namespaces' => true
		));
		$this->assertFalse($result);
	}

	/**
	 * Tests the loading of libraries
	 */
	public function testLibraryLoad() {
		$this->expectException('Failed to load class `SomeInvalidLibrary` from path ``.');
		Libraries::load('SomeInvalidLibrary', true);
	}

	/**
	 * Tests path caching by calling `path()` twice.
	 */
	public function testPathCaching() {
		$this->assertFalse(Libraries::cache(false));
		$path = Libraries::path(__CLASS__);
		$this->assertEqual(__FILE__, realpath($path));

		$result = Libraries::cache();
		$this->assertEqual(realpath($result[__CLASS__]), __FILE__);
	}

	public function testCacheControl() {
		$this->assertNull(Libraries::path('Foo'));
		$cache = Libraries::cache();
		Libraries::cache(array('Foo' => 'Bar'));
		$this->assertEqual('Bar', Libraries::path('Foo'));

		Libraries::cache(false);
		Libraries::cache($cache);
	}

	/**
	 * Tests recursive and non-recursive searching through libraries with paths.
	 */
	public function testFindingClasses() {
		$result = Libraries::find('lithium', array(
			'recursive' => true, 'path' => '/tests/cases', 'filter' => '/LibrariesTest/'
		));
		$this->assertIdentical(array(__CLASS__), $result);

		$result = Libraries::find('lithium', array(
			'path' => '/tests/cases/', 'filter' => '/LibrariesTest/'
		));
		$this->assertIdentical(array(), $result);

		$result = Libraries::find('lithium', array(
			'path' => '/tests/cases/core', 'filter' => '/LibrariesTest/'
		));
		$this->assertIdentical(array(__CLASS__), $result);

		$count = Libraries::find('lithium', array('recursive' => true));
		$count2 = Libraries::find(true, array('recursive' => true));
		$this->assertTrue($count < $count2);

		$result = Libraries::find('foo', array('recursive' => true));
		$this->assertNull($result);
	}

	public function testFindingClassesAndNamespaces() {
		$result = Libraries::find('lithium', array('namespaces' => true));
		$this->assertTrue(in_array('lithium\net', $result));
		$this->assertTrue(in_array('lithium\test', $result));
		$this->assertTrue(in_array('lithium\util', $result));
		$this->assertFalse(in_array('lithium\readme', $result));
		$this->assertFalse(in_array('lithium\readme.wiki', $result));
	}

	public function testFindingClassesWithExclude() {
		$options = array(
			'recursive' => true,
			'filter' => false,
			'exclude' => '/\w+Test$|webroot|index$|^app\\\\config|^\w+\\\\views\/|\./'
		);
		$classes = Libraries::find('lithium', $options);

		$this->assertTrue(in_array('lithium\util\Set', $classes));
		$this->assertTrue(in_array('lithium\util\Collection', $classes));
		$this->assertTrue(in_array('lithium\core\Libraries', $classes));
		$this->assertTrue(in_array('lithium\action\Dispatcher', $classes));

		$this->assertFalse(in_array('lithium\tests\integration\data\SourceTest', $classes));
		$this->assertFalse(preg_grep('/\w+Test$/', $classes));

		$expected = Libraries::find('lithium', array(
			'filter' => '/\w+Test$/', 'recursive' => true
		));
		$result = preg_grep('/\w+Test/', $expected);
		$this->assertEqual($expected, $result);
	}

	public function testServiceLocateAll() {
		$result = Libraries::locate('tests');
		$this->assertTrue(count($result) > 30);

		$expected = array(
			'lithium\template\view\adapter\File',
			'lithium\template\view\adapter\Simple'
		);
		$result = Libraries::locate('adapter.template.view');
		$this->assertEqual($expected, $result);

		$result = Libraries::locate('test.filter');
		$this->assertTrue(count($result) >= 4);
		$this->assertTrue(in_array('lithium\test\filter\Affected', $result));
		$this->assertTrue(in_array('lithium\test\filter\Complexity', $result));
		$this->assertTrue(in_array('lithium\test\filter\Coverage', $result));
		$this->assertTrue(in_array('lithium\test\filter\Profiler', $result));
	}

	public function testServiceLocateInstantiation() {
		$result = Libraries::instance('adapter.template.view', 'Simple');
		$this->assertTrue($result instanceof Simple);
		$this->expectException("Class `Foo` of type `adapter.template.view` not found.");
		$result = Libraries::instance('adapter.template.view', 'Foo');
	}

	public function testServiceLocateAllCommands() {
		$result = Libraries::locate('command');
		$this->assertTrue(count($result) > 7);

		$expected = array('lithium\console\command\g11n\Extract');
		$result = Libraries::locate('command.g11n');
		$this->assertEqual($expected, $result);
	}

	/**
	 * Tests locating service objects.  These tests may fail if not run on a stock install, as other
	 * objects may preceed the core objects in load order.
	 */
	public function testServiceLocation() {
		$this->assertNull(Libraries::locate('adapter', 'File'));
		$this->assertNull(Libraries::locate('adapter.view', 'File'));
		$this->assertNull(Libraries::locate('invalid_package', 'InvalidClass'));

		$result = Libraries::locate('adapter.template.view', 'File');
		$this->assertEqual('lithium\template\view\adapter\File', $result);

		$result = Libraries::locate('adapter.storage.cache', 'File');
		$expected = 'lithium\storage\cache\adapter\File';
		$this->assertEqual($expected, $result);

		$result = Libraries::locate('data.source', 'Database');
		$expected = 'lithium\data\source\Database';
		$this->assertEqual($expected, $result);

		$result = Libraries::locate('adapter.data.source.database', 'MySql');
		$expected = 'lithium\data\source\database\adapter\MySql';
		$this->assertEqual($expected, $result);

		$result = Libraries::locate(null, '\lithium\data\source\Database');
		$expected = '\lithium\data\source\Database';
		$this->assertEqual($expected, $result);

		$expected = new stdClass();
		$result = Libraries::locate(null, $expected);
		$this->assertEqual($expected, $result);
	}

	public function testServiceLocateApp() {
		$this->skipIf(!$this->hasApp, 'Running in standalone mode.');
		$result = Libraries::locate('controllers', 'HelloWorld');
		$expected = 'app\controllers\HelloWorldController';
		$this->assertEqual($expected, $result);

		// Tests caching of paths
		$result = Libraries::locate('controllers', 'HelloWorld');
		$this->assertEqual($expected, $result);
	}

	public function testServiceLocateCommand() {
		$result = Libraries::locate('command.g11n', 'Extract');
		$expected = 'lithium\console\command\g11n\Extract';
		$this->assertEqual($expected, $result);
	}

	public function testCaseSensitivePathLookups() {
		Libraries::cache(false);
		$library = Libraries::get('lithium');
		$base = $library['path'] . '/';

		$expected = realpath($base . 'template/View.php');

		$result = Libraries::path('\lithium\template\View');
		$this->assertEqual($expected, $result);

		$result = Libraries::path('lithium\template\View');
		$this->assertEqual($expected, $result);

		$expected = realpath($base . 'template/view');

		$result = Libraries::path('\lithium\template\view', array('dirs' => true));
		$this->assertEqual($expected, $result);

		$result = Libraries::path('lithium\template\view', array('dirs' => true));
		$this->assertEqual($expected, $result);
	}

	public function testPathDirectoryLookups() {
		$library = Libraries::get('lithium');
		$base = $library['path'] . '/';

		$result = Libraries::path('lithium\template\View', array('dirs' => true));
		$expected = realpath($base . 'template/View.php');
		$this->assertEqual($expected, $result);

		$result = Libraries::path('lithium\template\views', array('dirs' => true));
		$this->assertNull($result);
	}

	public function testFindingClassesWithCallableFilters() {
		$result = Libraries::find('lithium', array(
			'recursive' => true, 'path' => '/tests/cases', 'format' => function($file, $config) {
				return new SplFileInfo($file);
			},
			'filter' => function($file) {
				if ($file->getFilename() === 'LibrariesTest.php') {
					return $file;
				}
			}
		));
		$this->assertEqual(1, count($result));
		$this->assertIdentical(__FILE__, $result[0]->getRealPath());
	}

	public function testFindingClassesWithCallableExcludes() {
		$result = Libraries::find('lithium', array(
			'recursive' => true, 'path' => '/tests/cases',
			'format' => function($file, $config) {
				return new SplFileInfo($file);
			},
			'filter' => null,
			'exclude' =>  function($file) {
				if ($file->getFilename() == 'LibrariesTest.php') {
					return true;
				}
			}
		));
		$this->assertEqual(1, count($result));
		$this->assertIdentical(__FILE__, $result[0]->getRealPath());
	}

	public function testFindWithOptions() {
		$result = Libraries::find('lithium', array(
			'path' => '/console/command/create/template',
			'namespaces' => false,
			'suffix' => false,
			'filter' => false,
			'exclude' => false,
			'format' => function ($file, $config) {
				return basename($file);
			}
		));
		$this->assertTrue(count($result) > 3);
		$this->assertTrue(array_search('controller.txt.php', $result) !== false);
		$this->assertTrue(array_search('model.txt.php', $result) !== false);
		$this->assertTrue(array_search('plugin.phar.gz', $result) !== false);
	}

	public function testLocateWithDotSyntax() {
		$expected = 'lithium\template\helper\Html';
		$result = Libraries::locate('helper', 'lithium.Html');
		$this->assertEqual($expected, $result);
	}

	public function testLocateCommandInLithium() {
		$expected = array(
			'lithium\console\command\Create',
			'lithium\console\command\G11n',
			'lithium\console\command\Help',
			'lithium\console\command\Library',
			'lithium\console\command\Route',
			'lithium\console\command\Test'
		);
		$result = Libraries::locate('command', null, array(
			'library' => 'lithium', 'recursive' => false
		));
		$this->assertEqual($expected, $result);
	}

	public function testLocateCommandInLithiumRecursiveTrue() {
		$expected = array(
			'lithium\console\command\Create',
			'lithium\console\command\G11n',
			'lithium\console\command\Help',
			'lithium\console\command\Library',
			'lithium\console\command\Route',
			'lithium\console\command\Test',
			'lithium\console\command\g11n\Extract',
			'lithium\console\command\create\Controller',
			'lithium\console\command\create\Mock',
			'lithium\console\command\create\Model',
			'lithium\console\command\create\Test',
			'lithium\console\command\create\View'
		);
		$result = Libraries::locate('command', null, array(
			'library' => 'lithium', 'recursive' => true
		));
		$this->assertEqual($expected, $result);
	}

	public function testLocateWithLibrary() {
	    $expected = array();
	    $result = (array) Libraries::locate("tests", null, array('library' => 'doesntExist'));
	    $this->assertIdentical($expected, $result);
	}

	public function testLocateWithLithiumLibrary() {
	    $expected = (array) Libraries::find('lithium', array(
		    'path' => '/tests',
			'preFilter' => '/[A-Z][A-Za-z0-9]+\Test\./',
	        'recursive' => true,
	        'filter' => '/cases|integration|functional|mocks/'
	    ));
	    $result = (array) Libraries::locate("tests", null, array('library' => 'lithium'));
	    $this->assertEqual($expected, $result);
	}

	public function testLocateWithTestAppLibrary() {
		$testApp = Libraries::get(true, 'resources') . '/tmp/tests/test_app';
		mkdir($testApp, 0777, true);
		Libraries::add('test_app', array('path' => $testApp));

		mkdir($testApp . '/tests/cases/models', 0777, true);
		file_put_contents($testApp . '/tests/cases/models/UserTest.php',
		"<?php namespace test_app\\tests\\cases\\models;\n
			class UserTest extends \\lithium\\test\\Unit { public function testMe() {
				\$this->assertTrue(true);
			}}"
		);
		Libraries::cache(false);

		$expected = array('test_app\\tests\\cases\\models\\UserTest');
	    $result = (array) Libraries::locate("tests", null, array('library' => 'test_app'));
	    $this->assertEqual($expected, $result);

		$this->_cleanUp();
	}

	/**
	 * Tests that `Libraries::realPath()` correctly resolves paths to files inside Phar archives.
	 */
	public function testPathsInPharArchives() {
		$base = Libraries::get('lithium', 'path');
		$path = realpath("{$base}/console/command/create/template/app.phar.gz");

		$expected = "phar://{$path}/controllers/HelloWorldController.php";
		$result = Libraries::realPath($expected);
		$this->assertEqual($expected, $result);
	}

	public function testClassInstanceWithSubnamespace() {
		$testApp = Libraries::get(true, 'resources') . '/tmp/tests/test_app';
		mkdir($testApp);
		$paths = array("/controllers", "/controllers/admin");

		foreach ($paths as $path) {
			$namespace = str_replace('/', '\\', $path);
			$dotsyntax = str_replace('/', '.', trim($path, '/'));
			$class = 'Posts';

			Libraries::add('test_app', array('path' => $testApp));

			mkdir($testApp . $path, 0777, true);
			file_put_contents($testApp . $path . "/{$class}Controller.php",
			"<?php namespace test_app{$namespace};\n
				class {$class}Controller extends \\lithium\\action\\Controller {
				public function index() {
					return true;
				}}"
			);
			Libraries::cache(false);

			$expected = "test_app{$namespace}\\{$class}Controller";
			$instance = Libraries::instance($dotsyntax, "Posts", array('library' => 'test_app'));
		    $result = get_class($instance);
		    $this->assertEqual($expected, $result, "{$path} did not work");
		}

		$this->_cleanUp();
	}

	/**
	 * Tests that `Libraries::map()` and `Libraries::unmap()`
	 *
	 */
	public function testMapUnmap() {
		$testApp = Libraries::get(true, 'resources') . '/tmp/tests/test_app';
		mkdir($testApp, 0777, true);
		Libraries::add('test_app', array('path' => $testApp));

		mkdir($testApp. '/lib', 0777);
		mkdir($testApp. '/_patch', 0777);

		file_put_contents($testApp . '/lib/LibTest.php',
		"<?php namespace test_app\\lib;\n
			class LibTest{ public function testMe() {
				return 'core class';
			}}"
		);

		file_put_contents($testApp . '/_patch/PatchedLibTest.php',
		"<?php namespace test_app\\lib;\n
			class LibTest{ public function testMe() {
				return 'patched class';
			}}"
		);

		$expected = $result = Libraries::realPath($testApp . '/lib/LibTest.php');
		$result = Libraries::path('test_app\\lib\\LibTest');

		$this->assertEqual($expected, $result);

		Libraries::map(array(
			'test_app\\lib\\LibTest' => $testApp . '/_patch/PatchedLibTest.php'
		));

		$expected = $result = Libraries::realPath($testApp . '/_patch/PatchedLibTest.php');
		$result = Libraries::path('test_app\\lib\\LibTest');

		Libraries::unmap(array('test_app\\lib\\LibTest'));

		$expected = $result = Libraries::realPath($testApp . '/lib/LibTest.php');
		$result = Libraries::path('test_app\\lib\\LibTest');

		$this->assertEqual($expected, $result);

		Libraries::map(array(
			'test_app\\lib\\LibTest' => $testApp . '/_patch/PatchedLibTest.php'
		));
		Libraries::unmap('test_app\\lib\\LibTest');

		$expected = $result = Libraries::realPath($testApp . '/lib/LibTest.php');
		$result = Libraries::path('test_app\\lib\\LibTest');

		Libraries::map(array(
			'test_app\\lib\\LibTest' => $testApp . '/_patch/PatchedLibTest.php'
		));

		$object = new \test_app\lib\LibTest();

		$result = $object->testMe();
		$this->assertEqual('patched class', $result);

		$this->_cleanUp();
	}
}

?>