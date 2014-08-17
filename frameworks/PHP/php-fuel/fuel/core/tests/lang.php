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
 * Lang class tests
 *
 * @group Core
 * @group Lang
 */
class Test_Lang extends TestCase
{

	/**
	 * Test for Lang::get()
	 *
	 * @test
	 */
	public function test_line()
	{
		Lang::load('test');
		$output = Lang::get('hello', array('name' => 'Bob'));
		$expected = 'Hello there Bob!';
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Lang::get()
	 *
	 * @test
	 */
	public function test_line_invalid()
	{
		Lang::load('test');
		$output = Lang::get('non_existant_hello', array('name' => 'Bob'));
		$expected = false;
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Lang::set()
	 *
	 * @test
	 */
	public function test_set_return_true()
	{
		$output = Lang::set('testing_set_valid', 'Ahoy :name!');
		$this->assertNull($output);
	}

	/**
	 * Test for Lang::set()
	 *
	 * @test
	 */
	public function test_set()
	{
		Lang::set('testing_set_valid', 'Ahoy :name!');
		$output = Lang::get('testing_set_valid', array('name' => 'Bob'));
		$expected = 'Ahoy Bob!';
		$this->assertEquals($expected, $output);
	}
}
