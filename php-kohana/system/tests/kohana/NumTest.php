<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Tests Num
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.num
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @author     BRMatt <matthew@sigswitch.com>
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_NumTest extends Unittest_TestCase
{
	protected $default_locale;

	/**
	 * SetUp test enviroment
	 */
	// @codingStandardsIgnoreStart
	public function setUp()
	// @codingStandardsIgnoreEnd
	{
		parent::setUp();

		setlocale(LC_ALL, 'en_US.utf8');
	}

	/**
	 * Tear down environment
	 */
	// @codingStandardsIgnoreStart
	public function tearDown()
	// @codingStandardsIgnoreEnd
	{
		parent::tearDown();

		setlocale(LC_ALL, $this->default_locale);
	}

	/**
	 * Provides test data for test_bytes()
	 *
	 * @return array
	 */
	public function provider_bytes()
	{
		return array(
			array(204800.0, '200K'),
			array(5242880.0, '5MiB'),
			array(1000.0, 1000),
			array(2684354560.0, '2.5GB'),
		);
	}
	
	/**
	 * Tests Num::bytes()
	 *
	 * @test
	 * @covers Num::bytes
	 * @dataProvider provider_bytes
	 * @param integer Expected Value
	 * @param string  Input value
	 */
	public function test_bytes($expected, $size)
	{
		$this->assertSame($expected, Num::bytes($size));
	}
	
	/**
	 * Provides test data for test_ordinal()
	 * @return array
	 */
	public function provider_ordinal()
	{
		return array(
			array(0, 'th'),
			array(1, 'st'),
			array(21, 'st'),
			array(112, 'th'),
			array(23, 'rd'),
			array(42, 'nd'),
		);
	}

	/**
	 *
	 * @test
	 * @dataProvider provider_ordinal
	 * @param integer $number
	 * @param <type> $expected
	 */
	public function test_ordinal($number, $expected)
	{
		$this->assertSame($expected, Num::ordinal($number));
	}

	/**
	 * Provides test data for test_format()
	 * @return array
	 */
	public function provider_format()
	{
		return array(
			// English
			array(10000, 2, FALSE, '10,000.00'),
			array(10000, 2, TRUE, '10,000.00'),

			// Additional dp's should be removed
			array(123.456, 2, FALSE, '123.46'),
			array(123.456, 2, TRUE, '123.46'),
		);
	}

	/**
	 * @todo test locales
	 * @test
	 * @dataProvider provider_format
	 * @param integer $number
	 * @param integer $places
	 * @param boolean $monetary
	 * @param string $expected
	 */
	public function test_format($number, $places, $monetary, $expected)
	{
		$this->assertSame($expected, Num::format($number, $places, $monetary));
	}

	/**
	 * Provides data for test_round()
	 * @return array
	 */
	function provider_round()
	{
		return array(
			array(5.5, 0, array(
				6.0,
				5.0,
				6.0,
				5.0,
			)),
			array(42.5, 0, array(
				43.0,
				42.0,
				42.0,
				43.0,
			)),
			array(10.4, 0, array(
				10.0,
				10.0,
				10.0,
				10.0,
			)),
			array(10.8, 0, array(
				11.0,
				11.0,
				11.0,
				11.0,
			)),
			array(-5.5, 0, array(
				-6.0,
				-5.0,
				-6.0,
				-5.0,
			)),
			array(-10.5, 0, array(
				-11.0,
				-10.0,
				-10.0,
				-11.0,
			)),
			array(26.12375, 4, array(
				26.1238,
				26.1237,
				26.1238,
				26.1237,
			)),
			array(26.12325, 4, array(
				26.1233,
				26.1232,
				26.1232,
				26.1233,
			)),
		);
	}

	/**
	 * @test
	 * @dataProvider provider_round
	 * @param number $input
	 * @param integer $precision
	 * @param integer $mode
	 * @param number $expected
	 */
	function test_round($input, $precision, $expected)
	{
		foreach (array(Num::ROUND_HALF_UP, Num::ROUND_HALF_DOWN, Num::ROUND_HALF_EVEN, Num::ROUND_HALF_ODD) as $i => $mode)
		{
			$this->assertSame($expected[$i], Num::round($input, $precision, $mode, FALSE));
		}
	}
}
