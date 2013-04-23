<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\g11n;

use lithium\g11n\Multibyte;
use lithium\tests\mocks\g11n\multibyte\adapter\MockAdapter;

class MultibyteTest extends \lithium\test\Unit {

	protected $_backup = array();

	public $adapter;

	public function setUp() {
		$this->_backup['multibyteConfig'] = Multibyte::config();
		Multibyte::reset();

		$this->adapter = new MockAdapter();
		Multibyte::config(array('default' => array('object' => $this->adapter)));
	}

	public function tearDown() {
		Multibyte::reset();
		Multibyte::config($this->_backup['multibyteConfig']);
	}

	public function testIs() {
		$result = Multibyte::is('äbc');
		$this->assertTrue($result);

		$result = Multibyte::is('κόσμε');
		$this->assertTrue($result);

		$result = Multibyte::is("κό\nσμε");
		$this->assertTrue($result);

		$result = Multibyte::is("ab\xe9");
		$this->assertFalse($result);
	}

	public function testIsQuick() {
		$result = Multibyte::is('äbc', array('quick' => true));
		$this->assertTrue($result);

		$result = Multibyte::is('κόσμε', array('quick' => true));
		$this->assertTrue($result);

		$result = Multibyte::is("κό\nσμε", array('quick' => true));
		$this->assertTrue($result);
	}

	/**
	 * Verifies the behavior of `Multibyte::is()` when dealing with valid,
	 * invalid UTF-8 strings as well as edge cases. This test uses the stress
	 * test created by Markus Kuhn.
	 *
	 * This test is "special" in that it doesn't prove that the method returns
	 * correct results in any case - it shows how it actually behaves. It is no
	 * requirement that the method successfully detects each and any string as
	 * valid/invalid UTF-8. But: following a list which could be seen as goals
	 * we'd like to achieve. Please adapt the list when modifying the method.
	 * Any modification should result in getting closer to our goals not adding
	 * more to them ;)
	 *
	 * These items should be detected as valid UTF-8 (but currently aren't):
	 *  - lines 70, 74, 75 in section `First possible sequence of a certain length`.
	 *  - lines 79, 82, 83, 84 in section `Last possible sequence of a certain length`.
	 *
	 * @link http://www.cl.cam.ac.uk/~mgk25/
	 * @link http://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt
	 */
	public function testIsBehavioral() {
		$path = LITHIUM_LIBRARY_PATH . '/lithium/tests/resources/utf8_decoder_stress_test.txt';
		$data = file($path);

		$items = array(
			64 => true,
			70 => false,
			71 => true,
			72 => true,
			73 => true,
			74 => false,
			75 => false,
			79 => false,
			80 => true,
			81 => true,
			82 => false,
			83 => false,
			84 => false,
			101 => false,
			102 => false,
			104 => false,
			105 => false,
			106 => false,
			107 => false,
			108 => false,
			109 => false,
			113 => false,
			114 => false,
			115 => false,
			116 => false,
			123 => false,
			124 => false,
			129 => false,
			134 => false,
			139 => false,
			144 => false,
			154 => false,
			155 => false,
			156 => false,
			157 => false,
			158 => false,
			159 => false,
			160 => false,
			161 => false,
			168 => false,
			174 => false,
			176 => false,
			206 => false,
			207 => false,
			208 => false,
			209 => false,
			210 => false,
			219 => false,
			220 => false,
			221 => false,
			222 => false,
			223 => false,
			231 => false,
			232 => false,
			233 => false,
			234 => false,
			235 => false,
			246 => false,
			247 => false,
			248 => false,
			249 => false,
			250 => false,
			251 => false,
			252 => false,
			256 => false,
			257 => false,
			258 => false,
			259 => false,
			260 => false,
			261 => false,
			262 => false,
			263 => false,
			267 => true,
			268 => true
		);
		foreach ($items as $number => $expected) {
			$result = Multibyte::is($data[$number]);
			$message  = "Expected item on line {$number} to be detected as ";
			$message .= ($expected ? 'valid' : 'invalid') . " UTF-8.\n";
			$this->assertEqual($expected, $result, $message);
		}
	}

	/**
	 * Verifies the behavior of `Multibyte::is()` when dealing with valid,
	 * invalid UTF-8 strings as well as edge cases. Please see the docblock for
	 * `testIsBehaviroral` for more contextual information on the type of test
	 * and data used here.
	 *
	 * This test clearly shows and accepts the limitations in which the `quick`
	 * mode operates. The `quick` mode will obviously never get as good results
	 * as the normal one.
	 *
	 * These items should be detected as *invalid* UTF-8 (but currently aren't):
	 *  - lines 101-263 in nearly all remaining sections.
	 *
	 * @see lithium\tests\cases\g11n\MultibyteTest::testIsBehavioral()
	 */
	public function testIsQuickBehavioral() {
		$path = LITHIUM_LIBRARY_PATH . '/lithium/tests/resources/utf8_decoder_stress_test.txt';
		$data = file($path);

		$items = array(
			64 => true,
			70 => true,
			71 => true,
			72 => true,
			73 => true,
			74 => true,
			75 => true,
			79 => true,
			80 => true,
			81 => true,
			82 => true,
			83 => true,
			84 => true,
			101 => true,
			102 => true,
			104 => true,
			105 => true,
			106 => true,
			107 => true,
			108 => true,
			109 => true,
			113 => true,
			114 => true,
			115 => true,
			116 => true,
			123 => true,
			124 => true,
			129 => true,
			134 => true,
			139 => true,
			144 => true,
			154 => true,
			155 => true,
			156 => true,
			157 => true,
			158 => true,
			159 => true,
			160 => true,
			161 => true,
			168 => true,
			174 => true,
			176 => true,
			206 => true,
			207 => true,
			208 => true,
			209 => true,
			210 => true,
			219 => true,
			220 => true,
			221 => true,
			222 => true,
			223 => true,
			231 => true,
			232 => true,
			233 => true,
			234 => true,
			235 => true,
			246 => true,
			247 => true,
			248 => true,
			249 => true,
			250 => true,
			251 => true,
			252 => true,
			256 => true,
			257 => true,
			258 => true,
			259 => true,
			260 => true,
			261 => true,
			262 => true,
			263 => true,
			267 => true,
			268 => true
		);
		foreach ($items as $number => $expected) {
			$result = Multibyte::is($data[$number], array('quick' => true));
			$message  = "Expected item on line {$number} to be detected as ";
			$message .= ($expected ? 'valid' : 'invalid') . " UTF-8.\n";
			$this->assertEqual($expected, $result, $message);
		}
	}

	public function testStrlen() {
		Multibyte::strlen('test');

		$result = $this->adapter->testStrlenArgs;
		$expected = array('test');
		$this->assertEqual($expected, $result);
	}

	public function testStrpos() {
		Multibyte::strpos('abcab', 'c');

		$result = $this->adapter->testStrposArgs;
		$expected = array('abcab', 'c', 0);
		$this->assertEqual($expected, $result);

		Multibyte::strpos('abcab', 'c', 23);

		$result = $this->adapter->testStrposArgs;
		$expected = array('abcab', 'c', 23);
		$this->assertEqual($expected, $result);
	}

	public function testStrrpos() {
		Multibyte::strrpos('abcab', 'c');

		$result = $this->adapter->testStrrposArgs;
		$expected = array('abcab', 'c');
		$this->assertEqual($expected, $result);
	}

	public function testSubstr() {
		Multibyte::substr('abcab', 1);

		$result = $this->adapter->testSubstrArgs;
		$expected = array('abcab', 1, null);
		$this->assertEqual($expected, $result);

		Multibyte::substr('abcab', 1, 2);

		$result = $this->adapter->testSubstrArgs;
		$expected = array('abcab', 1, 2);
		$this->assertEqual($expected, $result);
	}
}

?>