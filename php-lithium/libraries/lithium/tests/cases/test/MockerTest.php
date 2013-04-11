<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\test;

use lithium\test\Mocker;

/**
 * WARNING:
 * No unit test should mock the same test as another to avoid conflicting filters.
 */
class MockerTest extends \lithium\test\Unit {

	public function setUp() {
		Mocker::register();
	}

	public function testAutoloadRegister() {
		Mocker::register();
		$registered = spl_autoload_functions();
		$this->assertTrue(in_array(array(
			'lithium\test\Mocker',
			'create'
		), $registered));
	}

	public function testBasicCreation() {
		$mockee = 'lithium\console\command\Mock';
		Mocker::create($mockee);
		$this->assertTrue(class_exists($mockee));
	}

	public function testBasicCreationExtendsCorrectParent() {
		$mocker = 'lithium\console\Request';
		$mockeeObj = new \lithium\console\request\Mock();
		$this->assertTrue(is_a($mockeeObj, $mocker));
	}

	public function testCannotMockNonLithiumClasses() {
		$mockee = 'stdClass\Mock';
		Mocker::create($mockee);
		$this->assertTrue(!class_exists($mockee));
	}

	public function testCannotCreateNonStandardMockClass() {
		$mockee = 'lithium\console\request\Mocker';
		Mocker::create($mockee);
		$this->assertTrue(!class_exists($mockee));
	}

	public function testFilteringNonStaticClass() {
		$dispatcher = new \lithium\console\dispatcher\Mock();

		$originalResult = $dispatcher->config(array());

		$dispatcher->applyFilter('config', function($self, $params, $chain) {
			return array();
		});

		$filteredResult = $dispatcher->config(array());

		$this->assertEqual(0, count($filteredResult));
		$this->assertNotEqual($filteredResult, $originalResult);
	}

	public function testFilteringNonStaticClassCanReturnOriginal() {
		$response = new \lithium\console\response\Mock();

		$originalResult = $response->styles();

		$response->applyFilter('styles', function($self, $params, $chain) {
			return $chain->next($self, $params, $chain);
		});

		$filteredResult = $response->styles();

		$this->assertEqual($filteredResult, $originalResult);
	}

	public function testFilteringStaticClass() {
		$mockee = 'lithium\analysis\parser\Mock';

		$code = 'echo "foobar";';

		$originalResult = $mockee::tokenize($code, array('wrap' => true));

		$mockee::applyFilter('tokenize', function($self, $params, $chain) {
			return array();
		});

		$filteredResult = $mockee::tokenize($code, array('wrap' => true));

		$this->assertEqual(0, count($filteredResult));
		$this->assertNotEqual($filteredResult, $originalResult);
	}

	public function testFilteringStaticClassCanReturnOriginal() {
		$mockee = 'lithium\analysis\inspector\Mock';

		$originalResult = $mockee::methods('lithium\analysis\Inspector');

		$mockee::applyFilter('tokenize', function($self, $params, $chain) {
			return $chain->next($self, $params, $chain);
		});

		$filteredResult = $mockee::methods('lithium\analysis\Inspector');

		$this->assertEqual($filteredResult, $originalResult);
	}

	public function testOriginalMethodNotCalled() {
		$http = new \lithium\tests\mocks\security\auth\adapter\mockHttp\Mock;

		$this->assertEqual(0, count($http->headers));

		$http->_writeHeader('Content-type: text/html');

		$this->assertEqual(1, count($http->headers));

		$http->applyFilter('_writeHeader', function($self, $params, $chain) {
			return false;
		});

		$http->_writeHeader('Content-type: application/pdf');

		$this->assertEqual(1, count($http->headers));
	}

	public function testFilteringAFilteredMethod() {
		$adapt = 'lithium\core\adaptable\Mock';
		$adapt::applyFilter('_initAdapter', function($self, $params, $chain) {
			return false;
		});
		$this->assertIdentical(false, $adapt::_initAdapter('foo', array()));
	}

	public function testStaticResults() {
		$docblock = 'lithium\analysis\docblock\Mock';
		$docblock::applyFilter(array('comment', 'tags'), function($self, $params, $chain) {
			return false;
		});
		$docblock::comment('foo', 'foobar');
		$docblock::comment('bar');
		$docblock::tags('baz');

		$this->assertIdentical(2, count($docblock::$results['comment']));
		$this->assertIdentical(array('foo', 'foobar'), $docblock::$results['comment'][0]['args']);
		$this->assertIdentical(false, $docblock::$results['comment'][0]['result']);
		$this->assertIdentical(array('bar'), $docblock::$results['comment'][1]['args']);
		$this->assertIdentical(false, $docblock::$results['comment'][1]['result']);

		$this->assertIdentical(1, count($docblock::$results['tags']));
		$this->assertIdentical(array('baz'), $docblock::$results['tags'][0]['args']);
		$this->assertIdentical(false, $docblock::$results['tags'][0]['result']);
	}

	public function testInstanceResults() {
		$debugger = new \lithium\data\schema\Mock;
		$debugger->applyFilter(array('names', 'meta'), function($self, $params, $chain) {
			return false;
		});
		$debugger->names('foo', 'foobar');
		$debugger->names('bar');
		$debugger->meta('baz');

		$this->assertIdentical(2, count($debugger->results['names']));
		$this->assertIdentical(array('foo', 'foobar'), $debugger->results['names'][0]['args']);
		$this->assertIdentical(false, $debugger->results['names'][0]['result']);
		$this->assertIdentical(array('bar'), $debugger->results['names'][1]['args']);
		$this->assertIdentical(false, $debugger->results['names'][1]['result']);

		$this->assertIdentical(1, count($debugger->results['meta']));
		$this->assertIdentical(array('baz'), $debugger->results['meta'][0]['args']);
		$this->assertIdentical(false, $debugger->results['meta'][0]['result']);
	}

	public function testSkipByReference() {
		$stdObj = new \lithium\tests\mocks\test\mockStdClass\Mock();
		$stdObj->foo = 'foo';
		$originalData = $stdObj->data();
		$stdObj->applyFilter('data', function($self, $params, $chain) {
			return array();
		});
		$nonfilteredData = $stdObj->data();
		$this->assertIdentical($originalData, $nonfilteredData);
	}

	public function testGetByReference() {
		$stdObj = new \lithium\tests\mocks\test\mockStdClass\Mock();
		$stdObj->foo = 'foo';
		$foo =& $stdObj->foo;
		$foo = 'bar';
		$this->assertIdentical('bar', $stdObj->foo);
	}

	public function testChainReturnsMockerChain() {
		$this->assertTrue(Mocker::chain(new \stdClass) instanceof \lithium\test\MockerChain);
	}

}

?>