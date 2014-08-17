<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');
/**
 * Tests Kohana_UTF8 class
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.utf8
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_UTF8Test extends Unittest_TestCase
{

	/**
	 * Provides test data for test_clean()
	 */
	public function provider_clean()
	{
		return array(
			array("\0", ''),
			array("→foo\021", '→foo'),
			array("\x7Fbar", 'bar'),
			array("\xFF", ''),
			array("\x41", 'A'),
			array(array("→foo\021", "\x41"), array('→foo', 'A')),
		);
	}

	/**
	 * Tests UTF8::clean
	 *
	 * @test
	 * @dataProvider provider_clean
	 */
	public function test_clean($input, $expected)
	{
		$this->assertSame($expected, UTF8::clean($input));
	}

	/**
	 * Provides test data for test_is_ascii()
	 */
	public function provider_is_ascii()
	{
		return array(
			array("\0", TRUE),
			array("\$eno\r", TRUE),
			array('Señor', FALSE),
			array(array('Se', 'nor'), TRUE),
			array(array('Se', 'ñor'), FALSE),
		);
	}

	/**
	 * Tests UTF8::is_ascii
	 *
	 * @test
	 * @dataProvider provider_is_ascii
	 */
	public function test_is_ascii($input, $expected)
	{
		$this->assertSame($expected, UTF8::is_ascii($input));
	}

	/**
	 * Provides test data for test_strip_ascii_ctrl()
	 */
	public function provider_strip_ascii_ctrl()
	{
		return array(
			array("\0", ''),
			array("→foo\021", '→foo'),
			array("\x7Fbar", 'bar'),
			array("\xFF", "\xFF"),
			array("\x41", 'A'),
		);
	}

	/**
	 * Tests UTF8::strip_ascii_ctrl
	 *
	 * @test
	 * @dataProvider provider_strip_ascii_ctrl
	 */
	public function test_strip_ascii_ctrl($input, $expected)
	{
		$this->assertSame($expected, UTF8::strip_ascii_ctrl($input));
	}

	/**
	 * Provides test data for test_strip_non_ascii()
	 */
	public function provider_strip_non_ascii()
	{
		return array(
			array("\0\021\x7F", "\0\021\x7F"),
			array('I ♥ cocoñùт', 'I  coco'),
		);
	}

	/**
	 * Tests UTF8::strip_non_ascii
	 *
	 * @test
	 * @dataProvider provider_strip_non_ascii
	 */
	public function test_strip_non_ascii($input, $expected)
	{
		$this->assertSame($expected, UTF8::strip_non_ascii($input));
	}

	/**
	 * Provides test data for test_transliterate_to_ascii()
	 */
	public function provider_transliterate_to_ascii()
	{
		return array(
			array('Cocoñùт', -1, 'Coconuт'),
			array('COCOÑÙТ', -1, 'COCOÑÙТ'),
			array('Cocoñùт', 0, 'Coconuт'),
			array('COCOÑÙТ', 0, 'COCONUТ'),
			array('Cocoñùт', 1, 'Cocoñùт'),
			array('COCOÑÙТ', 1, 'COCONUТ'),
		);
	}

	/**
	 * Tests UTF8::transliterate_to_ascii
	 *
	 * @test
	 * @dataProvider provider_transliterate_to_ascii
	 */
	public function test_transliterate_to_ascii($input, $case, $expected)
	{
		$this->assertSame($expected, UTF8::transliterate_to_ascii($input, $case));
	}

	/**
	 * Provides test data for test_strlen()
	 */
	public function provider_strlen()
	{
		return array(
			array('Cocoñùт', 7),
			array('Coconut', 7),
		);
	}

	/**
	 * Tests UTF8::strlen
	 *
	 * @test
	 * @dataProvider provider_strlen
	 */
	public function test_strlen($input, $expected)
	{
		$this->assertSame($expected, UTF8::strlen($input));
	}

	/**
	 * Provides test data for test_strpos()
	 */
	public function provider_strpos()
	{
		return array(
			array('Cocoñùт', 'o', 0, 1),
			array('Cocoñùт', 'ñ', 1, 4),
		);
	}

	/**
	 * Tests UTF8::strpos
	 *
	 * @test
	 * @dataProvider provider_strpos
	 */
	public function test_strpos($input, $str, $offset, $expected)
	{
		$this->assertSame($expected, UTF8::strpos($input, $str, $offset));
	}

	/**
	 * Provides test data for test_strrpos()
	 */
	public function provider_strrpos()
	{
		return array(
			array('Cocoñùт', 'o', 0, 3),
			array('Cocoñùт', 'ñ', 2, 4),
		);
	}

	/**
	 * Tests UTF8::strrpos
	 *
	 * @test
	 * @dataProvider provider_strrpos
	 */
	public function test_strrpos($input, $str, $offset, $expected)
	{
		$this->assertSame($expected, UTF8::strrpos($input, $str, $offset));
	}

	/**
	 * Provides test data for test_substr()
	 */
	public function provider_substr()
	{
		return array(
			array('Cocoñùт', 3, 2, 'oñ'),
			array('Cocoñùт', 3, 9, 'oñùт'),
			array('Cocoñùт', 3, NULL, 'oñùт'),
			array('Cocoñùт', 3, -2, 'oñ'),
		);
	}

	/**
	 * Tests UTF8::substr
	 *
	 * @test
	 * @dataProvider provider_substr
	 */
	public function test_substr($input, $offset, $length, $expected)
	{
		$this->assertSame($expected, UTF8::substr($input, $offset, $length));
	}

	/**
	 * Provides test data for test_substr_replace()
	 */
	public function provider_substr_replace()
	{
		return array(
			array('Cocoñùт', 'šš', 3, 2, 'Cocššùт'),
			array('Cocoñùт', 'šš', 3, 9, 'Cocšš'),
		);
	}

	/**
	 * Tests UTF8::substr_replace
	 *
	 * @test
	 * @dataProvider provider_substr_replace
	 */
	public function test_substr_replace($input, $replacement, $offset, $length, $expected)
	{
		$this->assertSame($expected, UTF8::substr_replace($input, $replacement, $offset, $length));
	}

	/**
	 * Provides test data for test_strtolower()
	 */
	public function provider_strtolower()
	{
		return array(
			array('COCOÑÙТ', 'cocoñùт'),
			array('JÄGER',   'jäger'),
		);
	}

	/**
	 * Tests UTF8::strtolower
	 *
	 * @test
	 * @dataProvider provider_strtolower
	 */
	public function test_strtolower($input, $expected)
	{
		$this->assertSame($expected, UTF8::strtolower($input));
	}

	/**
	 * Provides test data for test_strtoupper()
	 */
	public function provider_strtoupper()
	{
		return array(
			array('Cocoñùт', 'COCOÑÙТ'),
			array('jäger',   'JÄGER'),
		);
	}

	/**
	 * Tests UTF8::strtoupper
	 *
	 * @test
	 * @dataProvider provider_strtoupper
	 */
	public function test_strtoupper($input, $expected)
	{
		$this->assertSame($expected, UTF8::strtoupper($input));
	}

	/**
	 * Provides test data for test_ucfirst()
	 */
	public function provider_ucfirst()
	{
		return array(
			array('ñùт', 'Ñùт'),
		);
	}

	/**
	 * Tests UTF8::ucfirst
	 *
	 * @test
	 * @dataProvider provider_ucfirst
	 */
	public function test_ucfirst($input, $expected)
	{
		$this->assertSame($expected, UTF8::ucfirst($input));
	}

	/**
	 * Provides test data for test_strip_non_ascii()
	 */
	public function provider_ucwords()
	{
		return array(
			array('ExAmple', 'ExAmple'),
			array('i ♥ Cocoñùт', 'I ♥ Cocoñùт'),
		);
	}

	/**
	 * Tests UTF8::ucwords
	 *
	 * @test
	 * @dataProvider provider_ucwords
	 */
	public function test_ucwords($input, $expected)
	{
		$this->assertSame($expected, UTF8::ucwords($input));
	}

	/**
	 * Provides test data for test_strcasecmp()
	 */
	public function provider_strcasecmp()
	{
		return array(
			array('Cocoñùт',   'Cocoñùт', 0),
			array('Čau',       'Čauo',   -1),
			array('Čau',       'Ča',      1),
			array('Cocoñùт',   'Cocoñ',   4),
			array('Cocoñùт',   'Coco',    6),
		);
	}

	/**
	 * Tests UTF8::strcasecmp
	 *
	 * @test
	 * @dataProvider provider_strcasecmp
	 */
	public function test_strcasecmp($input, $input2, $expected)
	{
		$this->assertSame($expected, UTF8::strcasecmp($input, $input2));
	}

	/**
	 * Provides test data for test_str_ireplace()
	 */
	public function provider_str_ireplace()
	{
		return array(
			array('т', 't', 'cocoñuт', 'cocoñut'),
			array('Ñ', 'N', 'cocoñuт', 'cocoNuт'),
			array(array('т', 'Ñ', 'k' => 'k'), array('t', 'N', 'K'), array('cocoñuт'), array('cocoNut')),
			array(array('ñ'), 'n', 'cocoñuт', 'coconuт'),
		);
	}

	/**
	 * Tests UTF8::str_ireplace
	 *
	 * @test
	 * @dataProvider provider_str_ireplace
	 */
	public function test_str_ireplace($search, $replace, $subject, $expected)
	{
		$this->assertSame($expected, UTF8::str_ireplace($search, $replace, $subject));
	}

	/**
	 * Provides test data for test_stristr()
	 */
	public function provider_stristr()
	{
		return array(
			array('Cocoñùт',   'oñ', 'oñùт'),
			array('Cocoñùт',   'o', 'ocoñùт'),
			array('Cocoñùт',   'k', FALSE),
		);
	}

	/**
	 * Tests UTF8::stristr
	 *
	 * @test
	 * @dataProvider provider_stristr
	 */
	public function test_stristr($input, $input2, $expected)
	{
		$this->assertSame($expected, UTF8::stristr($input, $input2));
	}

	/**
	 * Provides test data for test_strspn()
	 */
	public function provider_strspn()
	{
		return array(
			array("foo", "o", 1, 2, 2),
			array('Cocoñùт', 'oñ', NULL, NULL, 1),
			array('Cocoñùт', 'oñ', 2, 4, 1),
			array('Cocoñùт', 'šš', 3, 9, 4),
		);
	}

	/**
	 * Tests UTF8::strspn
	 *
	 * @test
	 * @dataProvider provider_strspn
	 */
	public function test_strspn($input, $mask, $offset, $length, $expected)
	{
		$this->assertSame($expected, UTF8::strspn($input, $mask, $offset, $length));
	}

	/**
	 * Provides test data for test_strcspn()
	 */
	public function provider_strcspn()
	{
		return array(
			array('Cocoñùт', 'oñ', NULL, NULL, 1),
			array('Cocoñùт', 'oñ', 2, 4, 1),
			array('Cocoñùт', 'šš', 3, 9, 4),
		);
	}

	/**
	 * Tests UTF8::strcspn
	 *
	 * @test
	 * @dataProvider provider_strcspn
	 */
	public function test_strcspn($input, $mask, $offset, $length, $expected)
	{
		$this->assertSame($expected, UTF8::strcspn($input, $mask, $offset, $length));
	}

	/**
	 * Provides test data for test_str_pad()
	 */
	public function provider_str_pad()
	{
		return array(
			array('Cocoñùт', 10, 'š', STR_PAD_RIGHT, 'Cocoñùтššš'),
			array('Cocoñùт', 10, 'š', STR_PAD_LEFT,  'šššCocoñùт'),
			array('Cocoñùт', 10, 'š', STR_PAD_BOTH,  'šCocoñùтšš'),
		);
	}

	/**
	 * Tests UTF8::str_pad
	 *
	 * @test
	 * @dataProvider provider_str_pad
	 */
	public function test_str_pad($input, $length, $pad, $type, $expected)
	{
		$this->assertSame($expected, UTF8::str_pad($input, $length, $pad, $type));
	}

        /**
	 * Tests UTF8::str_pad error
	 *
	 * @test
	 * @expectedException UTF8_Exception
	 */
	public function test_str_pad_error()
	{
		UTF8::str_pad('Cocoñùт', 10, 'š', 15,  'šCocoñùтšš');
	}

	/**
	 * Provides test data for test_str_split()
	 */
	public function provider_str_split()
	{
		return array(
			array('Bár',     1, array('B', 'á', 'r')),
			array('Cocoñùт', 2, array('Co', 'co', 'ñù', 'т')),
			array('Cocoñùт', 3, array('Coc', 'oñù', 'т')),
		);
	}

	/**
	 * Tests UTF8::str_split
	 *
	 * @test
	 * @dataProvider provider_str_split
	 */
	public function test_str_split($input, $split_length, $expected)
	{
		$this->assertSame($expected, UTF8::str_split($input, $split_length));
	}

	/**
	 * Provides test data for test_strrev()
	 */
	public function provider_strrev()
	{
		return array(
			array('Cocoñùт', 'тùñocoC'),
		);
	}

	/**
	 * Tests UTF8::strrev
	 *
	 * @test
	 * @dataProvider provider_strrev
	 */
	public function test_strrev($input, $expected)
	{
		$this->assertSame($expected, UTF8::strrev($input));
	}

	/**
	 * Provides test data for test_trim()
	 */
	public function provider_trim()
	{
		return array(
			array(' bar ', NULL, 'bar'),
			array('bar',   'b',  'ar'),
			array('barb',  'b',  'ar'),
		);
	}

	/**
	 * Tests UTF8::trim
	 *
	 * @test
	 * @dataProvider provider_trim
	 */
	public function test_trim($input, $input2, $expected)
	{
		$this->assertSame($expected, UTF8::trim($input, $input2));
	}

	/**
	 * Provides test data for test_ltrim()
	 */
	public function provider_ltrim()
	{
		return array(
			array(' bar ', NULL, 'bar '),
			array('bar',   'b',  'ar'),
			array('barb',  'b',  'arb'),
			array('ñùт',   'ñ',  'ùт'),
		);
	}

	/**
	 * Tests UTF8::ltrim
	 *
	 * @test
	 * @dataProvider provider_ltrim
	 */
	public function test_ltrim($input, $charlist, $expected)
	{
		$this->assertSame($expected, UTF8::ltrim($input, $charlist));
	}

	/**
	 * Provides test data for test_rtrim()
	 */
	public function provider_rtrim()
	{
		return array(
			array(' bar ', NULL, ' bar'),
			array('bar',   'b',  'bar'),
			array('barb',  'b',  'bar'),
			array('Cocoñùт',  'т',  'Cocoñù'),
		);
	}

	/**
	 * Tests UTF8::rtrim
	 *
	 * @test
	 * @dataProvider provider_rtrim
	 */
	public function test_rtrim($input, $input2, $expected)
	{
		$this->assertSame($expected, UTF8::rtrim($input, $input2));
	}

	/**
	 * Provides test data for test_ord()
	 */
	public function provider_ord()
	{
		return array(
			array('f', 102),
			array('ñ', 241),
			array('Ñ', 209),
		);
	}

	/**
	 * Tests UTF8::ord
	 *
	 * @test
	 * @dataProvider provider_ord
	 */
	public function test_ord($input, $expected)
	{
		$this->assertSame($expected, UTF8::ord($input));
	}
}
