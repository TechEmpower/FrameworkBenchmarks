<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Fuel\Core;

/**
 * Numeric helper tests
 *
 * @package		Fuel
 * @category	Core
 * @author      Chase "Syntaqx" Hutchins
 *
 * @group Core
 * @group Num
 */
class Test_Num extends TestCase
{

	/**
	 * @see     Num::bytes
	 */
	public function test_bytes()
	{
		$output = Num::bytes('200K');
		$expected = '204800';

		$this->assertEquals($expected, $output);
	}

	/**
	 * @see     Num::bytes
	 * @expectedException Exception
	 */
	public function test_bytes_exception()
	{
		$output = Num::bytes('invalid');
	}

	/**
	 * @see     Num::format_bytes
	 */
	public function test_format_bytes()
	{
		$output = Num::format_bytes('204800');
		$expected = '200 kB';

		$this->assertEquals($expected, $output);

		$output = Num::format_bytes('invalid');
		$this->assertFalse($output);
	}

	/**
	 * @see     Num::quantity
	 */
	public function test_quantity()
	{
		// Return the same
		$output = Num::quantity('100');
		$expected = '100';

		$this->assertEquals($expected, $output);

		$output = Num::quantity('7500');
		$expected = '8K';

		$this->assertEquals($expected, $output);

		$output = Num::quantity('1500000');
		$expected = '2M';

		$this->assertEquals($expected, $output);


		$output = Num::quantity('1000000000');
		$expected = '1B';

		$this->assertEquals($expected, $output);

		// Get current localized decimal separator
		$locale_conv = localeconv();
		$decimal_point = isset($locale_conv['decimal_point']) ? $locale_conv['decimal_point'] : '.';

		$output = Num::quantity('7500', 1);
		$expected = '7'.$decimal_point.'5K';

		$this->assertEquals($expected, $output);
	}

	/**
	 * @see     Num::format
	 */
	public function test_format()
	{
		$output = Num::format('1234567890', '(000) 000-0000');
		$expected = '(123) 456-7890';

		$this->assertEquals($expected, $output);

		$output = Num::format(null, '(000) 000-0000');
		$this->assertNull($output);

		$output = Num::format('1234567890', null);
		$expected = '1234567890';

		$this->assertEquals($expected, $output);
	}

	/**
	 * @see     Num::mask_string
	 */
	public function test_mask_string()
	{
		$output = Num::mask_string('1234567812345678', '**** - **** - **** - 0000', ' -');
		$expected = '**** - **** - **** - 5678';

		$this->assertEquals($expected, $output);

		// Return the same
		$output = Num::mask_string('1234567812345678');
		$expected = '1234567812345678';

		$this->assertEquals($expected, $output);
	}

	/**
	 * @see     Num::format_phone
	 */
	public function test_format_phone()
	{
		$output = Num::format_phone('1234567890');
		$expected = '(123) 456-7890';

		$this->assertEquals($expected, $output);
	}

	/**
	 * @see     Num::smart_format_phone
	 */
	public function test_smart_format_phone()
	{
		$output = Num::smart_format_phone('1234567');
		$expected = '123-4567';

		$this->assertEquals($expected, $output);

		// Return the same
		$output = Num::smart_format_phone('123456');
		$expected = '123456';

		$this->assertEquals($expected, $output);
	}

	/**
	 * @see     Num::format_exp
	 */
	public function test_format_exp()
	{
		$output = Num::format_exp('1234');
		$expected = '12-34';

		$this->assertEquals($expected, $output);
	}

	/**
	 * @see     Num::mask_credit_card
	 */
	public function test_mask_credit_card()
	{
		$output = Num::mask_credit_card('1234567812345678');
		$expected = '**** **** **** 5678';

		$this->assertEquals($expected, $output);
	}
}

/* End of file num.php */
