<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\test;

use Exception;
use lithium\core\Libraries;
use lithium\tests\mocks\test\MockUnitTest;
use lithium\tests\mocks\test\cases\MockSkipThrowsException;
use lithium\tests\mocks\test\cases\MockTestErrorHandling;
use lithium\tests\mocks\test\cases\MockSetUpThrowsException;
use lithium\tests\mocks\test\cases\MockTearDownThrowsException;

class UnitTest extends \lithium\test\Unit {

	public $test;

	public function setUp() {
		$this->test = new MockUnitTest();
	}

	public function testBaseAssertions() {
		$this->test->assert(true);
		$this->test->assert(false);

		$results = $this->test->results();
		$result = array_pop($results);
		$this->assertEqual('fail', $result['result']);

		$this->assertTrue(true);
		$this->assertFalse(false);
	}

	public function testCompareIsEqual() {
		$result = $this->test->compare('equal', 'string', 'string');
		$this->assertTrue($result);
	}

	public function testCompareIsIdentical() {
		$result = $this->test->compare('identical', 'string', 'string');
		$this->assertTrue($result);
	}

	public function testCompareTypes() {
		$expected = array(
			'trace' => null,
			'expected' => "(array) Array\n(\n)",
			'result' => "(string) string"
		);
		$result = $this->test->compare('equal', array(), 'string');
		$this->assertEqual($expected, $result);
	}

	public function testAssertEqualNumeric() {
		$expected = array(1, 2, 3);
		$result = array(1, 2, 3);
		$this->test->assertEqual($expected, $result);

		$expected = 'pass';
		$results = $this->test->results();
		$this->assertEqual($expected, $results[0]['result']);
	}

	public function testAssertEqualNumericFail() {
		$expected = array(1, 2, 3);
		$result = array(1, 2);
		$this->test->assertEqual($expected, $result);
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected = "trace: [2]\nexpected: 3\nresult: NULL\n";
		$this->assertEqual($expected, $results[0]['message']);

		$expected = array(
			'trace' => '[2]',
			'expected' => 3,
			'result' => null
		);
		$this->assertEqual($expected, $results[0]['data']);
	}

	public function testAssertBacktraces() {
		$this->test->testSomething();
		$results = $this->test->results();

		$expected = 'assert';
		$this->assertEqual($expected, $results[0]['assertion']);

		$expected = 'lithium\\tests\\mocks\\test\\MockUnitTest';
		$this->assertEqual($expected, $results[0]['class']);

		$expected = 'testSomething';
		$this->assertEqual($expected, $results[0]['method']);

		$expected = 25;
		$this->assertEqual($expected, $results[0]['line']);
	}

	public function testAssertEqualAssociativeArray() {
		$expected = array(
			'expected' => 'array',
			'result' => 'string'
		);
		$result = array(
			'expected' => 'array',
			'result' => 'string'
		);
		$this->test->assertEqual($expected, $result);

		$expected = 'pass';
		$results = $this->test->results();
		$this->assertEqual($expected, $results[0]['result']);
	}

	public function testAssertEqualThreeDFail() {
		$expected = array(
			array(array(1, 2), array(1, 2)),
			array(array(1, 2), array(1, 2))
		);
		$result = array(
			array(array(1, 2), array(1)),
			array(array(1, 2), array(1))
		);
		$this->test->assertEqual($expected, $result);
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected  = "trace: [0][1][1]\nexpected: 2\nresult: NULL\n";
		$expected .= "trace: [1][1][1]\nexpected: 2\nresult: NULL\n";
		$this->assertEqual($expected, $results[0]['message']);

		$expected = array(
			array(
				array(
					'trace' => '[0][1][1]',
					'expected' => 2,
					'result' => null
				)
			),
			array(
				array(
					'trace' => '[1][1][1]',
					'expected' => 2,
					'result' => null
				)
			)
		);
		$this->assertEqual($expected, $results[0]['data']);
	}

	public function testAssertWithCustomMessage() {
		$expected = false;
		$result = true;
		$this->test->assertEqual($expected, $result, 'Custom Message Test');

		$expected = 'Custom Message Test';
		$results = $this->test->results();
		$this->assertEqual($expected, $results[0]['message']);
	}

	public function testSubject() {
		$expected = 'lithium\\tests\\mocks\\test\\MockUnit';
		$result = $this->test->subject();
		$this->assertEqual($expected, $result);
	}

	public function testRun() {
		$file = realpath(LITHIUM_LIBRARY_PATH) . '/lithium/tests/mocks/test/MockUnitTest.php';
		$expected = array(
			'result' => 'pass',
			'class' => 'lithium\\tests\\mocks\\test\\MockUnitTest',
			'method' => 'testNothing',
			'message' => "expected: true\nresult: true\n",
			'data' => array('expected' => true, 'result' => true),
			'file' => realpath($file),
			'line' => 14,
			'assertion' => 'assertTrue'
		);
		$result = $this->test->run();
		$this->assertEqual($expected, $result[0]);
	}

	public function testFail() {
		$this->test->fail('Test failed.');
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected = 'Test failed.';
		$this->assertEqual($expected, $results[0]['message']);
	}

	public function testAssertNotEqual() {
		$expected = true;
		$result = true;
		$this->test->assertNotEqual($expected, $result);
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);
	}

	public function testAssertIdentical() {
		$expected = true;
		$result = 1;
		$this->test->assertIdentical($expected, $result);
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);
	}

	public function testAssertIdenticalArray() {
		$expected = array('1', '2', '3');
		$result = array('1', '3', '4');
		$this->test->assertIdentical($expected, $result);
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected = "trace: [1]\nexpected: '2'\nresult: '3'\n";
		$this->assertEqual($expected, $results[0]['message']);
	}

	public function testAssertNull() {
		$expected = null;
		$result = null;
		$this->test->assertNull($expected, $result);
		$results = $this->test->results();

		$expected = 'pass';
		$this->assertEqual($expected, $results[0]['result']);
	}

	public function testAssertNoPattern() {
		$expected = '/\s/';
		$result = null;
		$this->test->assertNoPattern($expected, $result);
		$results = $this->test->results();

		$expected = 'pass';
		$this->assertEqual($expected, $results[0]['result']);
	}

	public function testAssertPattern() {
		$expected = '/\s/';
		$result = ' ';
		$this->test->assertPattern($expected, $result);
		$results = $this->test->results();

		$expected = 'pass';
		$this->assertEqual($expected, $results[0]['result']);
	}

	public function testAssertTags() {
		$result = '<input id="test">';
		$this->test->assertTags($result, array(
			'input' => array('id' => 'test')
		));
		$results = $this->test->results();

		$expected = 'pass';
		$this->assertEqual($expected, $results[0]['result']);
	}

	public function testAssertTagsNoClosingTag() {
		$result = '<span id="test">';
		$this->test->assertTags($result, array(
			'span' => array('id' => 'test'), '/span'
		));
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected = '- Item #2 / regex #3 failed: Close span tag';
		$this->assertEqual($expected, $results[0]['message']);
	}

	public function testAssertTagsMissingAttribute() {
		$result = '<span></span>';
		$this->test->assertTags($result, array(
			'span' => array('id' => 'test'), '/span'
		));
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected = '- Item #1 / regex #1 failed: Attribute "id" == "test"';
		$this->assertEqual($expected, $results[0]['message']);
	}

	public function testAssertTagsString() {
		$result = '<span>ok</span>';
		$this->test->assertTags($result, array('<span'));
		$results = $this->test->results();

		$expected = 'pass';
		$this->assertEqual($expected, $results[0]['result']);
	}

	public function testAssertTagsFailTextEqual() {
		$result = '<span>ok</span>';
		$this->test->assertTags($result, array('span'));
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected = '- Item #1 / regex #0 failed: Text equals "span"';
		$this->assertEqual($expected, $results[0]['message']);
	}

	public function testAssertException() {
		$closure = function() {
			throw new Exception('Test exception message.');
		};

		$expected = 'Test exception message.';
		$this->test->assertException($expected, $closure);
		$results = $this->test->results();

		$expected = 'pass';
		$this->assertEqual($expected, $results[0]['result']);

		$expected = 'Exception';
		$this->test->assertException($expected, $closure);
		$results = $this->test->results();

		$expected = 'pass';
		$this->assertEqual($expected, $results[1]['result']);

		$expected = '/Test/';
		$this->test->assertException($expected, $closure);
		$results = $this->test->results();

		$expected = 'pass';
		$this->assertEqual($expected, $results[2]['result']);
	}

	public function testAssertExceptionNotThrown() {
		$closure = function() {};
		$expected = 'Exception';
		$this->test->assertException($expected, $closure);
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected = 'An exception "Exception" was expected but not thrown.';
		$this->assertEqual($expected, $results[0]['message']);
	}

	public function testAssertExceptionWrongException() {
		$closure = function() {
			throw new Exception('incorrect');
		};

		$expected = 'correct';
		$this->test->assertException($expected, $closure);
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected  = 'Exception "correct" was expected. Exception "Exception" ';
		$expected .= 'with message "incorrect" was thrown instead.';
		$this->assertEqual($expected, $results[0]['message']);
	}

	public function testIdenticalArrayFail() {
		$expected = array('1', '2', '3');
		$result = array(1, '2', '3');;
		$this->test->assertIdentical($expected, $result);
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected = "trace: [0]\nexpected: '(string) 1'\nresult: '(integer) 1'\n";
		$this->assertEqual($expected, $results[0]['message']);
	}

	public function testCleanUp() {
		$base = Libraries::get(true, 'resources') . '/tmp/tests';
		$this->skipIf(!is_writable($base), "Path `{$base}` is not writable.");

		$this->assertTrue(mkdir("{$base}/cleanup_test"));
		$this->assertTrue(touch("{$base}/cleanup_test/file"));
		$this->assertTrue(touch("{$base}/cleanup_test/.hideme"));

		$this->_cleanUp();
		$this->assertFalse(file_exists("{$base}/cleanup_test"));
	}

	public function testCleanUpWithFullPath() {
		$base = Libraries::get(true, 'resources') . '/tmp/tests';
		$this->skipIf(!is_writable($base), "Path `{$base}` is not writable.");

		$this->assertTrue(mkdir("{$base}/cleanup_test"));
		$this->assertTrue(touch("{$base}/cleanup_test/file"));
		$this->assertTrue(touch("{$base}/cleanup_test/.hideme"));

		$this->_cleanUp("{$base}/cleanup_test");
		$this->assertTrue(file_exists("{$base}/cleanup_test"));
		$this->assertFalse(file_exists("{$base}/cleanup_test/file"));
		$this->assertFalse(file_exists("{$base}/cleanup_test/.hideme"));

		$this->_cleanUp();
	}

	public function testCleanUpWithRelativePath() {
		$base = Libraries::get(true, 'resources') . '/tmp/tests';
		$this->skipIf(!is_writable($base), "Path `{$base}` is not writable.");

		$this->assertTrue(mkdir("{$base}/cleanup_test"));
		$this->assertTrue(touch("{$base}/cleanup_test/file"));
		$this->assertTrue(touch("{$base}/cleanup_test/.hideme"));

		$this->_cleanUp("tests/cleanup_test");
		$this->assertTrue(file_exists("{$base}/cleanup_test"));
		$this->assertFalse(file_exists("{$base}/cleanup_test/file"));
		$this->assertFalse(file_exists("{$base}/cleanup_test/.hideme"));

		$this->_cleanUp();
	}

	public function testSkipIf() {
		try {
			$this->test->skipIf(true, 'skip me');
		} catch (Exception $e) {
			$result = $e->getMessage();
		}
		$expected = 'skip me';
		$this->assertEqual($expected, $result);
	}

	public function testExpectException() {
		$this->test->expectException('test expected exception');
		$results = $this->test->results();

		$expected = 'test expected exception';
		$result = $this->test->expected();
		$this->assertEqual($expected, $result[0]);
	}

	public function testHandleException() {
		$this->test->handleException(new Exception('test handle exception'));
		$results = $this->test->results();

		$expected = 'test handle exception';
		$this->assertEqual($expected, $results[0]['message']);
	}

	public function testExpectExceptionRegex() {
		$this->test->expectException('/test handle exception/');
		$this->test->handleException(new Exception('test handle exception'));

		$this->assertFalse($this->test->expected());
	}

	public function testExpectExceptionPostNotThrown() {
		$this->test->run(array(
			'methods' => array(
				'prepareTestExpectExceptionNotThrown'
			)
		));
		$results = $this->test->results();
		$message = 'expectException in a method with no exception should result in a failed test.';

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result'], $message);

		$expected = 'Expected exception matching `test` uncaught.';
		$this->assertEqual($expected, $results[0]['message']);
	}

	public function testGetTest() {
		$expected = __CLASS__;
		$result = static::get('lithium\test\Unit');
		$this->assertEqual($expected, $result);
	}

	/**
	 * With a fresh PHP environment this might throw an exception:
	 * `strtotime(): It is not safe to rely on the system's timezone settings. You are
	 * *required* to use the date.timezone setting or the date_default_timezone_set() function.`
	 * See also http://www.php.net/manual/en/function.date-default-timezone-get.php
	 */
	public function testAssertCookie() {
		$expected = array(
			'key' => 'key2.nested', 'value' => 'value1', 'expires' => 'May 04 2010 14:02:36 EST'
		);
		$this->test->assertCookie($expected);
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected = '/not found in headers./';
		$this->assertPattern($expected, $results[0]['message']);
	}

	public function testAssertCookieWithHeaders() {
		$headers = array(
			'Set-Cookie: name[key]=value; expires=Tue, 04-May-2010 19:02:36 GMT; path=/',
			'Set-Cookie: name[key1]=value1; expires=Tue, 04-May-2010 19:02:36 GMT; path=/',
			'Set-Cookie: name[key2][nested]=value1; expires=Tue, 04-May-2010 19:02:36 GMT; path=/'
		);

		$this->test->assertCookie(array('key' => 'key', 'value' => 'value'), $headers);
		$this->test->assertCookie(array('key' => 'key1', 'value' => 'value1'), $headers);
		$this->test->assertCookie(array('key' => 'key2.nested', 'value' => 'value1'), $headers);

		$expected = array(
			'key' => 'key2.nested', 'value' => 'value1', 'expires' => 'May 04 2010 14:02:36 EST'
		);
		$this->test->assertCookie($expected, $headers);

		$results = $this->test->results();

		$expected = 'pass';
		$this->assertEqual($expected, $results[0]['result']);
		$this->assertEqual($expected, $results[1]['result']);
		$this->assertEqual($expected, $results[2]['result']);
		$this->assertEqual($expected, $results[3]['result']);
	}

	public function testCompareWithEmptyResult() {
		$result = $this->test->compare('equal', array('key' => array('val1', 'val2')), array());

		$expected = array(
			'trace' => '[key]',
			'expected' => array('val1', 'val2'),
			'result' => array()
		);
		$this->assertEqual($expected, $result);
	}

	public function testExceptionCatching() {
		$test = new MockSkipThrowsException();
		$test->run();
		$expected = 'skip throws exception';
		$results = $test->results();
		$this->assertEqual($expected, $results[0]['message']);

		$test = new MockSetUpThrowsException();
		$test->run();
		$expected = 'setUp throws exception';
		$results = $test->results();
		$this->assertEqual($expected, $results[0]['message']);

		$test = new MockTearDownThrowsException();
		$test->run();
		$expected = 'tearDown throws exception';
		$results = $test->results();
		$this->assertEqual($expected, $results[1]['message']);
	}

	public function testErrorHandling() {
		$test = new MockTestErrorHandling();

		$test->run();

		$expected = '/Missing argument 1/';
		$results = $test->results();
		$this->assertPattern($expected, $results[0]['message']);

		$expected = '/Unit::_arrayPermute()/';
		$this->assertPattern($expected, $results[0]['message']);
	}

	public function testAssertObjects() {
		$expected = (object) array('one' => 'two');
		$result = (object) array('one' => 'not-two');
		$this->test->assertEqual($expected, $result);
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected = 'one';
		$this->assertEqual($expected, $results[0]['data']['trace']);
	}

	public function testAssertArrayIdentical() {
		$expected = array('one' => array('one'));
		$result = array('one' => array());
		$this->test->assertIdentical($expected, $result);
		$results = $this->test->results();

		$expected = 'fail';
		$this->assertEqual($expected, $results[0]['result']);

		$expected = '[one]';
		$this->assertEqual($expected, $results[0]['data']['trace']);
	}

	public function testCompareIdenticalArray() {
		$expected = array(
			'trace' => null,
			'expected' => array(),
			'result' => array('two', 'values')
		);
		$result = $this->test->compare('identical', array(), array('two', 'values'));
		$this->assertEqual($expected, $result);
	}

	public function testCompareIdenticalMixedArray() {
		$array1 = array(
			'command' => 'test',
			'action' => 'action',
			'args' => array(),
			'long' => 'something',
			'i' => 1
		);

		$array2 = array(
			'command' => 'test',
			'action' => 'action',
			'long' => 'something',
			'args' => array(),
			'i' => 1
		);

		$result = $this->test->compare('identical', $array1, $array2);
		$expected = array('trace' => null, 'expected' => $array1, 'result' => $array2);
		$this->assertEqual($expected, $result);
	}

	public function testCompareEqualNullArray() {
		$expected = array('trace' => null, 'expected' => array(), 'result' => array(null));
		$result = $this->test->compare('equal', array(), array(null));
		$this->assertEqual($expected, $result);
	}

	public function testCompareIdenticalNullArray() {
		$expected = array('trace' => null, 'expected' => array(), 'result' => array(null));
		$result = $this->test->compare('identical', array(), array(null));
		$this->assertEqual($expected, $result);
	}

	public function testResults() {
		$this->test->assertTrue(false);
		$this->test->assertTrue(false);
		$this->test->assertTrue(true);
		$this->test->assertTrue(true);

		$expected = 4;
		$result = count($this->test->results());
		$this->assertEqual($expected, $result);
	}

	public function testTestMethods() {
		$expected = array(
			'testNothing', 'testSomething'
		);
		$result = $this->test->methods();
		$this->assertIdentical($expected, $result);
	}

	public function testAssertCountTrue() {
		$this->assertTrue($this->test->assertCount(1, array('foo')));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertCountFalse() {
		$this->assertFalse($this->test->assertCount(2, array('foo', 'bar', 'bar')));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertIdentical(array(
			'expected' => 2,
			'result' => 3
		), $result['data']);
	}

	public function testAssertNotCountTrue() {
		$this->assertTrue($this->test->assertNotCount(2, array('foo', 'bar', 'bar')));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertNotCountFalse() {
		$this->assertFalse($this->test->assertNotCount(1, array('foo')));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertIdentical(array(
			'expected' => 1,
			'result' => 1
		), $result['data']);
	}

	public function testArrayHasKeyTrue() {
		$this->assertTrue($this->test->assertArrayHasKey('bar', array('bar' => 'baz')));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testArrayHasKeyFalse() {
		$this->assertFalse($this->test->assertArrayHasKey('foo', array('bar' => 'baz')));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertIdentical(array(
			'expected' => 'foo',
			'result' => array('bar' => 'baz')
		), $result['data']);
	}

	public function testArrayNotHasKeyTrue() {
		$this->assertTrue($this->test->assertArrayNotHasKey('foo', array('bar' => 'baz')));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testArrayNotHasKeyFalse() {
		$this->assertFalse($this->test->assertArrayNotHasKey('bar', array('bar' => 'baz')));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertIdentical(array(
			'expected' => 'bar',
			'result' => array('bar' => 'baz')
		), $result['data']);
	}

	public function testClassHasAttributeTrue() {
		$this->assertTrue($this->test->assertClassHasAttribute('name', '\ReflectionClass'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testClassHasAttributeFalse() {
		$this->assertFalse($this->test->assertClassHasAttribute('foo', '\ReflectionClass'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'foo',
			'result' => array(
				new \ReflectionProperty('ReflectionClass', 'name')
			)
		), $result['data']);
	}

	public function testClassHasAttributeWrongClassType() {
		$self =& $this;
		$this->assertException('InvalidArgumentException', function() use($self) {
			$self->test->assertClassHasAttribute('foo', new \stdClass);
		});
	}

	public function testClassHasAttributeClassNotFound() {
		$self =& $this;
		$this->assertException('ReflectionException', function() use($self) {
			$self->test->assertClassHasAttribute('foo', '\foo\bar\baz');
		});
	}

	public function testClassNotHasAttributeTrue() {
		$this->assertTrue($this->test->assertClassNotHasAttribute('foo', '\ReflectionClass'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testClassNotHasAttributeFalse() {
		$this->assertFalse($this->test->assertClassNotHasAttribute('name', '\ReflectionClass'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'name',
			'result' => array(
				new \ReflectionProperty('ReflectionClass', 'name')
			)
		), $result['data']);
	}

	public function testClassNotHasAttributeClassNotFound() {
		$self =& $this;
		$this->assertException('ReflectionException', function() use($self) {
			$self->test->assertClassNotHasAttribute('foo', '\foo\bar\baz');
		});
	}

	public function testClassNotHasAttributeWrongClassType() {
		$self =& $this;
		$this->assertException('InvalidArgumentException', function() use($self) {
			$self->test->assertClassNotHasAttribute('foo', new \stdClass);
		});
	}

	public function testClassHasStaticAttributeTrue() {
		$class = '\lithium\core\StaticObject';
		$this->assertTrue($this->test->assertClassHasStaticAttribute('_methodFilters', $class));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testClassHasStaticAttributeFalse() {
		$class = '\lithium\core\StaticObject';
		$this->assertFalse($this->test->assertClassHasStaticAttribute('foobar', $class));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'foobar',
			'result' => array(
				new \ReflectionProperty('lithium\\core\\StaticObject', '_methodFilters'),
				new \ReflectionProperty('lithium\\core\\StaticObject', '_parents')
			)
		), $result['data']);
	}

	public function testClassHasStaticAttributeClassNotFound() {
		$self =& $this;
		$this->assertException('ReflectionException', function() use($self) {
			$self->test->assertClassHasStaticAttribute('foo', '\foo\bar\baz');
		});
	}

	public function testClassNotHasStaticAttributeTrue() {
		$class = '\lithium\core\StaticObject';
		$this->assertTrue($this->test->assertClassNotHasStaticAttribute('foobar', $class));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testClassNotHasStaticAttributeFalse() {
		$class = '\lithium\core\StaticObject';
		$this->assertFalse($this->test->assertClassNotHasStaticAttribute('_methodFilters', $class));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => '_methodFilters',
			'result' => array(
				new \ReflectionProperty('lithium\\core\\StaticObject', '_methodFilters'),
				new \ReflectionProperty('lithium\\core\\StaticObject', '_parents')
			)
		), $result['data']);
	}

	public function testClassNotHasStaticAttributeClassNotFound() {
		$self =& $this;
		$this->assertException('ReflectionException', function() use($self) {
			$self->test->assertClassNotHasStaticAttribute('foo', '\foo\bar\baz');
		});
	}

	public function testAssertContainsStringInStrTrue() {
		$this->assertTrue($this->test->assertContains('foo', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertContainsStringInStrFalse() {
		$this->assertFalse($this->test->assertContains('baz', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'baz',
			'result' => 'foobar'
		), $result['data']);
	}

	public function testAssertContainsTrue() {
		$this->assertTrue($this->test->assertContains('bar', array('foo', 'bar', 'baz')));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertContainsFalse() {
		$this->assertFalse($this->test->assertContains('foobar', array('foo', 'bar', 'baz')));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'foobar',
			'result' => array(
				'foo', 'bar', 'baz'
			)
		), $result['data']);
	}

	public function testAssertNotContainsStringInStrTrue() {
		$this->assertTrue($this->test->assertNotContains('baz', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertNotContainsStringInStrFalse() {
		$this->assertFalse($this->test->assertNotContains('foo', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'foo',
			'result' => 'foobar'
		), $result['data']);
	}

	public function testAssertNotContainsTrue() {
		$this->assertTrue($this->test->assertNotContains('foobar', array('foo', 'bar', 'baz')));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertNotContainsFalse() {
		$this->assertFalse($this->test->assertNotContains('bar', array('foo', 'bar', 'baz')));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'bar',
			'result' => array(
				'foo', 'bar', 'baz'
			)
		), $result['data']);
	}

	public function testAssertContainsOnlyTrue() {
		$this->assertTrue($this->test->assertContainsOnly('int', array(1,2,3)));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertContainsOnlyFalse() {
		$this->assertFalse($this->test->assertContainsOnly('string', array(1,2,3)));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'string',
			'result' => array(
				1,2,3
			)
		), $result['data']);
	}

	public function testAssertNotContainsOnlyTrue() {
		$this->assertTrue($this->test->assertNotContainsOnly('string', array(1,2,3)));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertNotContainsOnlyFalse() {
		$this->assertFalse($this->test->assertNotContainsOnly('int', array(1,2,3)));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'int',
			'result' => array(
				1,2,3
			)
		), $result['data']);
	}

	public function testAssertContainsOnlyInstanceOfTrue() {
		$obj = new \stdClass;
		$this->assertTrue($this->test->assertContainsOnlyInstancesOf('stdClass', array($obj)));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertContainsOnlyInstanceOfFalse() {
		$obj = new \lithium\test\Unit;
		$this->assertFalse($this->test->assertContainsOnlyInstancesOf('stdClass', array($obj)));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'stdClass',
			'result' => array(
				0 => new \lithium\test\Unit
			)
		), $result['data']);
	}

	public function testAssertEmptyTrue() {
		$this->assertTrue($this->test->assertEmpty(array()));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertEmptyFalse() {
		$this->assertFalse($this->test->assertEmpty(array(1)));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => array(1),
			'result' => false
		), $result['data']);
	}

	public function testAssertNotEmptyTrue() {
		$this->assertTrue($this->test->assertNotEmpty(array(1)));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertNotEmptyFalse() {
		$this->assertFalse($this->test->assertNotEmpty(array()));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => array(),
			'result' => false
		), $result['data']);
	}

	public function testAssertFileEqualsTrue() {
		$file1 = __DIR__ . '/UnitTest.php';
		$file2 = __DIR__ . '/UnitTest.php';
		$this->assertTrue($this->test->assertFileEquals($file1, $file2));
	}

	public function testAssertFileEqualsFalse() {
		$file1 = __DIR__ . '/UnitTest.php';
		$file2 = __DIR__ . '/ReportTest.php';
		$this->assertFalse($this->test->assertFileEquals($file1, $file2));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => md5_file($file1),
			'result' => md5_file($file2)
		), $result['data']);
	}

	public function testAssertFileNotEqualsTrue() {
		$file1 = __DIR__ . '/UnitTest.php';
		$file2 = __DIR__ . '/ReportTest.php';
		$this->assertTrue($this->test->assertFileNotEquals($file1, $file2));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertFileNotEqualsFalse() {
		$file1 = __DIR__ . '/UnitTest.php';
		$file2 = __DIR__ . '/UnitTest.php';
		$this->assertFalse($this->test->assertFileNotEquals($file1, $file2));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => md5_file($file1),
			'result' => md5_file($file2)
		), $result['data']);
	}

	public function testAssertFileExistsTrue() {
		$file1 = __FILE__;
		$this->assertTrue($this->test->assertFileExists($file1));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertFileExistsFalse() {
		$file1 = __DIR__ . '/does/not/exist.txt';
		$this->assertFalse($this->test->assertFileExists($file1));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => __DIR__ . '/does/not/exist.txt',
			'result' => false
		), $result['data']);
	}

	public function testAssertFileNotExistsTrue() {
		$file1 = __DIR__ . '/does/not/exist.txt';
		$this->assertTrue($this->test->assertFileNotExists($file1));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertFileNotExistsFalse() {
		$file1 = __FILE__;
		$this->assertFalse($this->test->assertFileNotExists($file1));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => __FILE__,
			'result' => false
		), $result['data']);
	}

	public function testAssertGreaterThanTrue() {
		$this->assertTrue($this->test->assertGreaterThan(5, 3));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertGreaterThanFalse() {
		$this->assertFalse($this->test->assertGreaterThan(3, 5));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 3,
			'result' => 5
		), $result['data']);
	}

	public function testAssertGreaterThanOrEqualTrue() {
		$this->assertTrue($this->test->assertGreaterThanOrEqual(5, 5));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertGreaterThanOrEqualFalse() {
		$this->assertFalse($this->test->assertGreaterThanOrEqual(3, 5));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 3,
			'result' => 5
		), $result['data']);
	}

	public function testAssertLessThanTrue() {
		$this->assertTrue($this->test->assertLessThan(3, 5));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertLessThanFalse() {
		$this->assertFalse($this->test->assertLessThan(5, 3));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 5,
			'result' => 3
		), $result['data']);
	}

	public function testAssertLessThanOrEqualTrue() {
		$this->assertTrue($this->test->assertLessThanOrEqual(5, 5));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertLessThanOrEqualFalse() {
		$this->assertFalse($this->test->assertLessThanOrEqual(5, 3));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 5,
			'result' => 3
		), $result['data']);
	}

	public function testAssertInstanceOfTrue() {
		$this->assertTrue($this->test->assertInstanceOf('\stdClass', new \stdClass));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertInstanceOfFalse() {
		$this->assertFalse($this->test->assertInstanceOf('\ReflectionClass', new \stdClass));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => '\ReflectionClass',
			'result' => 'stdClass'
		), $result['data']);
	}

	public function testAssertNotInstanceOfTrue() {
		$this->assertTrue($this->test->assertNotInstanceOf('\ReflectionClass', new \stdClass));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertNotInstanceOfFalse() {
		$this->assertFalse($this->test->assertNotInstanceOf('\stdClass', new \stdClass));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => '\stdClass',
			'result' => 'stdClass'
		), $result['data']);
	}

	public function testAssertInternalTypeTrue() {
		$this->assertTrue($this->test->assertInternalType('string', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertInternalTypeFalse() {
		$this->assertFalse($this->test->assertInternalType('int', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'int',
			'result' => 'string'
		), $result['data']);
	}

	public function testAssertNotInternalTypeTrue() {
		$this->assertTrue($this->test->assertNotInternalType('int', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertNotInternalTypeFalse() {
		$this->assertFalse($this->test->assertNotInternalType('string', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'string',
			'result' => 'string'
		), $result['data']);
	}

	public function testAssertNotNullTrue() {
		$this->assertTrue($this->test->assertNotNull(1));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertNotNullFalse() {
		$this->assertFalse($this->test->assertNotNull(null));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => NULL,
			'actual' => 'NULL',
		), $result['data']);
	}

	public function testObjectHasAttributeTrue() {
		$obj = new \ReflectionClass(new \stdClass);
		$this->assertTrue($this->test->assertObjectHasAttribute('name', $obj));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testObjectHasAttributeFalse() {
		$obj = new \ReflectionClass(new \stdClass);
		$this->assertFalse($this->test->assertObjectHasAttribute('foo', $obj));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'foo',
			'result' => array(
				new \ReflectionProperty('ReflectionClass', 'name')
			)
		), $result['data']);
	}

	public function testObjectHasAttributeWrongClassType() {
		$self =& $this;
		$this->assertException('InvalidArgumentException', function() use($self) {
			$self->test->assertObjectHasAttribute('foo', '\stdClass');
		});
	}

	public function testObjectNotHasAttributeTrue() {
		$obj = new \ReflectionClass(new \stdClass);
		$this->assertTrue($this->test->assertObjectNotHasAttribute('foo', $obj));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testObjectNotHasAttributeFalse() {
		$obj = new \ReflectionClass(new \stdClass);
		$this->assertFalse($this->test->assertObjectNotHasAttribute('name', $obj));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'name',
			'result' => array(
				new \ReflectionProperty('ReflectionClass', 'name')
			)
		), $result['data']);
	}

	public function testObjectNotHasAttributeWrongClassType() {
		$self =& $this;
		$this->assertException('InvalidArgumentException', function() use($self) {
			$self->test->assertObjectNotHasAttribute('foo', 'new \stdClass');
		});
	}

	public function testAssertRegExpTrue() {
		$this->assertTrue($this->test->assertRegExp('/^foo/', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertRegExpFalse() {
		$this->assertFalse($this->test->assertRegExp('/^bar/', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => '/^bar/',
			'result' => array()
		), $result['data']);
	}

	public function testAssertNotRegExpTrue() {
		$this->assertTrue($this->test->assertNotRegExp('/^bar/', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertNotRegExpFalse() {
		$this->assertFalse($this->test->assertNotRegExp('/^foo/', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => '/^foo/',
			'result' => array('foo')
		), $result['data']);
	}

	public function testAssertStringMatchesFormatTrue() {
		$this->assertTrue($this->test->assertStringMatchesFormat('%d', '10'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertStringMatchesFormatFalse() {
		$this->assertFalse($this->test->assertStringMatchesFormat('%d', '10.555'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => '%d',
			'result' => array('10')
		), $result['data']);
	}

	public function testAssertStringNotMatchesFormatTrue() {
		$this->assertTrue($this->test->assertStringNotMatchesFormat('%d', '10.555'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertStringNotMatchesFormatFalse() {
		$this->assertFalse($this->test->assertStringNotMatchesFormat('%d', '10'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => '%d',
			'result' => array('10')
		), $result['data']);
	}

	public function testAssertStringEndsWithTrue() {
		$this->assertTrue($this->test->assertStringEndsWith('bar', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertStringEndsWithFalse() {
		$this->assertFalse($this->test->assertStringEndsWith('foo', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'foo',
			'result' => 'foobar'
		), $result['data']);
	}

	public function testAssertStringStartsWithTrue() {
		$this->assertTrue($this->test->assertStringStartsWith('foo', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('pass', $result['result']);
	}

	public function testAssertStringStartsWithFalse() {
		$this->assertFalse($this->test->assertStringStartsWith('bar', 'foobar'));

		$results = $this->test->results();
		$result = array_pop($results);

		$this->assertEqual('fail', $result['result']);
		$this->assertEqual(array(
			'expected' => 'bar',
			'result' => 'foobar'
		), $result['data']);
	}

}

?>