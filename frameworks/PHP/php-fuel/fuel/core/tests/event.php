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
 * Event class tests
 *
 * @group Core
 * @group Event
 */
class Test_Event extends TestCase
{

	/**
	 * Test for Event::register()
	 *
	 * @test
	 */
	public function test_register_valid()
	{
		$output = Event::register('test_register_valid', 'Str::upper');
		$this->assertTrue($output);
	}

	/**
	 * Test for Event::has_events()
	 *
	 * @test
	 */
	public function test_hasevents_valid()
	{
		$output = Event::has_events('test_register_valid');
		$this->assertTrue($output);
	}

	/**
	 * Test for Event::trigger()
	 *
	 * @test
	 */
	public function test_trigger_valid()
	{
		$output = Event::trigger('test_register_valid', 'text to upper');
		$this->assertEquals('TEXT TO UPPER', $output);
	}

	/**
	 * Test for Event::register()
	 *
	 * @test
	 */
	public function test_register_invalid()
	{
		$output = Event::register('test_register_invalid', 'Imaginary::callback');
		$this->assertFalse($output);
	}

	/**
	 * Test for Event::has_events()
	 *
	 * @test
	 */
	public function test_hasevents_invalid()
	{
		$output = Event::has_events('test_register_invalid');
		$this->assertFalse($output);
	}

	/**
	 * Test for Event::trigger()
	 *
	 * @test
	 */
	public function test_trigger_invalid()
	{
		$output = Event::trigger('test_register_invalid', 'text to upper');
		$this->assertEquals('', $output);
	}
}
