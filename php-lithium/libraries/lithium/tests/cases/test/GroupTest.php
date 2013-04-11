<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\test;

use lithium\test\Group;
use lithium\core\Libraries;
use lithium\util\Collection;
use lithium\tests\cases\data\ModelTest;
use lithium\tests\cases\core\ObjectTest;
use lithium\tests\cases\g11n\CatalogTest;
use lithium\tests\mocks\test\MockUnitTest;
use lithium\tests\mocks\test\cases\MockTest;
use lithium\tests\mocks\test\cases\MockTestErrorHandling;
use lithium\tests\mocks\test\cases\MockSkipThrowsException;
use lithium\tests\mocks\test\cases\MockSetUpThrowsException;
use lithium\tests\mocks\test\cases\MockTearDownThrowsException;

class GroupTest extends \lithium\test\Unit {

	public function testAdd() {
		$group = new Group();

		$expected = new Collection();
		$result = $group->tests();
		$this->assertEqual($expected, $result);
	}

	public function testAddCaseThroughConstructor() {
		$data = (array) "\lithium\\tests\mocks\\test";
		$group = new Group(compact('data'));

		$expected = new Collection(array('data' => array(
			new MockSetUpThrowsException(),
			new MockSkipThrowsException(),
			new MockTearDownThrowsException(),
			new MockTest(),
			new MockTestErrorHandling()
		)));
		$result = $group->tests();

		$this->assertEqual($expected, $result);
	}

	public function testAddEmpty() {
		$group = new Group();
		$group->add('');
		$group->add('\\');
		$group->add('foobar');
		$this->assertFalse($group->items());
	}

	public function testAddByString() {
		$group = new Group();
		$result = $group->add('lithium\tests\cases\g11n');
		$expected = array(
			'lithium\tests\cases\g11n\CatalogTest',
			'lithium\tests\cases\g11n\LocaleTest',
			'lithium\tests\cases\g11n\MessageTest',
			'lithium\tests\cases\g11n\MultibyteTest',
			'lithium\tests\cases\g11n\multibyte\adapter\IconvTest',
			'lithium\tests\cases\g11n\multibyte\adapter\IntlTest',
			'lithium\tests\cases\g11n\multibyte\adapter\MbstringTest',
			'lithium\tests\cases\g11n\catalog\AdapterTest',
			'lithium\tests\cases\g11n\catalog\adapter\CodeTest',
			'lithium\tests\cases\g11n\catalog\adapter\GettextTest',
			'lithium\tests\cases\g11n\catalog\adapter\MemoryTest',
			'lithium\tests\cases\g11n\catalog\adapter\PhpTest'
		);
		$this->assertEqual($expected, $result);

		$result = $group->add('lithium\tests\cases\data\ModelTest');
		$expected = array(
			'lithium\tests\cases\g11n\CatalogTest',
			'lithium\tests\cases\g11n\LocaleTest',
			'lithium\tests\cases\g11n\MessageTest',
			'lithium\tests\cases\g11n\MultibyteTest',
			'lithium\tests\cases\g11n\multibyte\adapter\IconvTest',
			'lithium\tests\cases\g11n\multibyte\adapter\IntlTest',
			'lithium\tests\cases\g11n\multibyte\adapter\MbstringTest',
			'lithium\tests\cases\g11n\catalog\AdapterTest',
			'lithium\tests\cases\g11n\catalog\adapter\CodeTest',
			'lithium\tests\cases\g11n\catalog\adapter\GettextTest',
			'lithium\tests\cases\g11n\catalog\adapter\MemoryTest',
			'lithium\tests\cases\g11n\catalog\adapter\PhpTest',
			'lithium\tests\cases\data\ModelTest'
		);
		$this->assertEqual($expected, $result);
	}

	public function testAddByMixedThroughConstructor() {
		$group = new Group(array('data' => array(
			'lithium\tests\cases\data\ModelTest', new ObjectTest()
		)));
		$expected = new Collection(array('data' => array(new ModelTest(), new ObjectTest())));
		$result = $group->tests();
		$this->assertEqual($expected, $result);
	}

	public function testTests() {
		$group = new Group();
		$expected = array(
			'lithium\tests\cases\g11n\CatalogTest'
		);
		$result = $group->add('lithium\tests\cases\g11n\CatalogTest');
		$this->assertEqual($expected, $result);

		$results = $group->tests();
		$this->assertTrue($results instanceof Collection);

		$results = $group->tests();
		$this->assertTrue($results->current() instanceof CatalogTest);
	}

	public function testAddEmptyTestsRun() {
		$group = new Group();
		$result = $group->add('lithium\tests\mocks\test\MockUnitTest');
		$expected = array('lithium\tests\mocks\test\MockUnitTest');
		$this->assertEqual($expected, $result);

		$results = $group->tests();
		$this->assertTrue($results instanceof Collection);
		$this->assertTrue($results->current() instanceof MockUnitTest);

		$results = $group->tests()->run();

		$expected = 'pass';
		$result = $results[0][0]['result'];
		$this->assertEqual($expected, $result);

		$expected = 'testNothing';
		$result = $results[0][0]['method'];
		$this->assertEqual($expected, $result);

		$expected = 'lithium\tests\mocks\test\MockUnitTest';
		$result = $results[0][0]['class'];
		$this->assertEqual($expected, $result);

		$expected = str_replace('\\', '/', LITHIUM_LIBRARY_PATH);
		$expected = realpath($expected . '/lithium/tests/mocks/test/MockUnitTest.php');
		$result = $results[0][0]['file'];
		$this->assertEqual($expected, $result);
	}

	public function testGroupAllForLithium() {
		Libraries::cache(false);
		$result = Group::all(array('library' => 'lithium'));
		$this->assertTrue(count($result) >= 60);
	}

	public function testAddTestAppGroup() {
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

		$expected = (array) Libraries::find('test_app', array(
			'recursive' => true,
			'path' => '/tests',
			'filter' => '/cases|integration|functional/'
		));

		Libraries::cache(false);

		$group = new Group();
		$result = $group->add('test_app');
		$this->assertEqual($expected, $result);

		Libraries::cache(false);
		$this->_cleanUp();
	}

	public function testRunGroupAllForTestApp() {
		$testApp = Libraries::get(true, 'resources') . '/tmp/tests/test_app';
		mkdir($testApp);
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
		$result = Group::all(array('library' => 'test_app'));
	    $this->assertEqual($expected, $result);

		Libraries::cache(false);
		$this->_cleanUp();
	}

	public function testRunGroupForTestAppModel() {
		$testApp = Libraries::get(true, 'resources') . '/tmp/tests/test_app';
		mkdir($testApp);
		Libraries::add('test_app', array('path' => $testApp));

		mkdir($testApp . '/tests/cases/models', 0777, true);
		file_put_contents($testApp . '/tests/cases/models/UserTest.php',
		"<?php namespace test_app\\tests\\cases\\models;\n
			class UserTest extends \\lithium\\test\\Unit { public function testMe() {
				\$this->assertTrue(true);
			}}"
		);
		Libraries::cache(false);

		$group = new Group(array('data' => array('\\test_app\\tests\\cases')));

		$expected = array('test_app\\tests\\cases\\models\\UserTest');
		$result = $group->to('array');
	    $this->assertEqual($expected, $result);

		$expected = 'pass';
		$result = $group->tests()->run();
	    $this->assertEqual($expected, $result[0][0]['result']);

		Libraries::cache(false);
		$this->_cleanUp();
	}
}

?>