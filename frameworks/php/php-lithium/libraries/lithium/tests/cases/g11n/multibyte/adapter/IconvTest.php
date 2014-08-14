<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\g11n\multibyte\adapter;

use lithium\g11n\multibyte\adapter\Iconv;

class IconvTest extends \lithium\test\Unit {

	public $adapter;

	public function skip() {
		$this->skipIf(!Iconv::enabled(), 'The `Iconv` adapter is not enabled.');
	}

	public function setUp() {
		$this->adapter = new Iconv();
	}

	public function testStrlen() {
		$data = 'äbc';
		$result = $this->adapter->strlen($data);
		$expected = 3;
		$this->assertEqual($expected, $result);
	}

	public function testStrlenAscii() {
		$data = 'abc';
		$result = $this->adapter->strlen($data);
		$expected = 3;
		$this->assertEqual($expected, $result);
	}

	public function testStrlenEmptyish() {
		$data = '';
		$result = $this->adapter->strlen($data);
		$expected = 0;
		$this->assertEqual($expected, $result);

		$data = ' ';
		$result = $this->adapter->strlen($data);
		$expected = 1;
		$this->assertEqual($expected, $result);

		$data = false;
		$result = $this->adapter->strlen($data);
		$expected = 0;
		$this->assertEqual($expected, $result);

		$data = null;
		$result = $this->adapter->strlen($data);
		$expected = 0;
		$this->assertEqual($expected, $result);

		$data = 0;
		$result = $this->adapter->strlen($data);
		$expected = 1;
		$this->assertEqual($expected, $result);

		$data = '0';
		$result = $this->adapter->strlen($data);
		$expected = 1;
		$this->assertEqual($expected, $result);
	}

	public function testStrlenInvalidTriggersError() {
		$this->expectException('/Detected an incomplete multibyte character in input string/');
		$data = "ab\xe9";
		$result = $this->adapter->strlen($data);
	}

	public function testStrpos() {
		$haystack = 'abäab';
		$needle = 'ä';
		$offset = 0;
		$result = $this->adapter->strpos($haystack, $needle, $offset);
		$expected = 2;
		$this->assertEqual($expected, $result);

		$haystack = 'abäab';
		$needle = 'X';
		$offset = 0;
		$result = $this->adapter->strpos($haystack, $needle, $offset);
		$this->assertFalse($result);
	}

	public function testStrposAscii() {
		$haystack = 'abcab';
		$needle = 'c';
		$offset = 0;
		$result = $this->adapter->strpos($haystack, $needle, $offset);
		$expected = 2;
		$this->assertEqual($expected, $result);
	}

	public function testStrposWithOffset() {
		$haystack = 'abäab';
		$needle = 'b';
		$offset = 0;
		$result = $this->adapter->strpos($haystack, $needle, $offset);
		$expected = 1;
		$this->assertEqual($expected, $result);

		$haystack = 'abäab';
		$needle = 'a';
		$offset = 1;
		$result = $this->adapter->strpos($haystack, $needle, $offset);
		$expected = 3;
		$this->assertEqual($expected, $result);
	}

	public function testStrposNeedleAsOrdinalIsNotApplied() {
		$haystack = 'abcab';
		$needle = 99;
		$offset = 0;
		$result = $this->adapter->strpos($haystack, $needle, $offset);
		$this->assertFalse($result);
	}

	public function testStrposTriggersError() {
		$haystack = "ab\xe9cab";
		$needle = 'c';
		$offset = 0;

		$this->expectException('/Detected an illegal character in input string/');
		$this->adapter->strpos($haystack, $needle, $offset);
	}

	public function testStrposInvalidOffset() {
		$haystack = 'abäab';
		$needle = 'a';
		$offset = -1;

		$this->expectException('/Offset not contained in string/');
		$this->adapter->strpos($haystack, $needle, $offset);
	}

	public function testStrrpos() {
		$haystack = 'abäab';
		$needle = 'ä';
		$result = $this->adapter->strrpos($haystack, $needle);
		$expected = 2;
		$this->assertEqual($expected, $result);

		$haystack = 'abäab';
		$needle = 'X';
		$result = $this->adapter->strrpos($haystack, $needle);
		$this->assertFalse($result);
	}

	public function testStrrposAscii() {
		$haystack = 'abcab';
		$needle = 'c';
		$result = $this->adapter->strrpos($haystack, $needle);
		$expected = 2;
		$this->assertEqual($expected, $result);
	}

	public function testStrrposWithOffset() {
		$haystack = 'abäab';
		$needle = 'b';
		$result = $this->adapter->strrpos($haystack, $needle);
		$expected = 4;
		$this->assertEqual($expected, $result);

		$haystack = 'abäab';
		$needle = 'a';
		$result = $this->adapter->strrpos($haystack, $needle);
		$expected = 3;
		$this->assertEqual($expected, $result);
	}

	public function testStrrposTriggersError() {
		$haystack = "ab\xe9cab";
		$needle = 'c';

		$this->expectException('/Detected an illegal character in input string/');
		$this->adapter->strrpos($haystack, $needle);
	}

	public function testSubstr() {
		$string = 'abäab';
		$start = 0;
		$length = 3;
		$result = $this->adapter->substr($string, $start, $length);
		$expected = 'abä';
		$this->assertEqual($expected, $result);

		$string = 'abäab';
		$start = 2;
		$length = 3;
		$result = $this->adapter->substr($string, $start, $length);
		$expected = 'äab';
		$this->assertEqual($expected, $result);

		$string = 'abäab';
		$start = -3;
		$length = 3;
		$result = $this->adapter->substr($string, $start, $length);
		$expected = 'äab';
		$this->assertEqual($expected, $result);
	}

	public function testSubstrAscii() {
		$string = 'abcab';
		$start = 0;
		$length = 3;
		$result = $this->adapter->substr($string, $start, $length);
		$expected = 'abc';
		$this->assertEqual($expected, $result);
	}

	public function testSubstrInvalidTriggersError() {
		$string = "ab\xe9cab";
		$start = 0;
		$length = 3;

		$this->expectException('/Detected an illegal character in input string/');
		$this->adapter->substr($string, $start, $length);
	}
}

?>