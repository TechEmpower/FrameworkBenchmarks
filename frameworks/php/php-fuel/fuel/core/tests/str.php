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
 * Str class tests
 *
 * @group Core
 * @group Str
 */
class Test_Str extends TestCase
{

	public function truncate_provider()
	{
		return array(
			array(15, 'Lorem ipsum dolor sit amet, consectetur adipiscing elit.'),
		);
	}

	/**
	 * Test for Str::truncate()
	 *
	 * @test
	 * @dataProvider truncate_provider
	 */
	public function test_truncate_plain($limit, $string)
	{
		$output = Str::truncate($string, $limit);
		$expected = 'Lorem ipsum dol...';
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Str::truncate()
	 *
	 * @test
	 * @dataProvider truncate_provider
	 */
	public function test_truncate_custom_continuation($limit, $string)
	{
		$output = Str::truncate($string, $limit, '..');
		$expected = 'Lorem ipsum dol..';
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Str::truncate()
	 *
	 * @test
	 * @dataProvider truncate_provider
	 */
	public function test_truncate_not_html($limit, $string)
	{
		$string = '<h1>'.$string.'</h1>';

		$output = Str::truncate($string, $limit, '...', false);
		$expected = '<h1>Lorem ipsum...';
		$this->assertEquals($expected, $output);

		$output = Str::truncate($string, $limit, '...', true);
		$expected = '<h1>Lorem ipsum dol...</h1>';
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Str::truncate()
	 *
	 * @test
	 * @dataProvider truncate_provider
	 */
	public function test_truncate_is_html($limit, $string)
	{
		$string = '<h1>'.$string.'</h1>';

		$output = Str::truncate($string, $limit, '...', true);
		$expected = '<h1>Lorem ipsum dol...</h1>';
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Str::truncate()
	 *
	 * @test
	 * @dataProvider truncate_provider
	 */
	public function test_truncate_multiple_tags($limit, $string)
	{
		$limit = 400;
		$string = '<p><strong>'.$string.'</strong></p>';

		$output = Str::truncate($string, $limit, '...', true);
		$this->assertEquals($string, $output);
	}

	/**
	 * Test for Str::increment()
	 *
	 * @test
	 */
	public function test_increment()
	{
		$values = array('valueA', 'valueB', 'valueC');

		for ($i = 0; $i < count($values); $i ++)
		{
			$output = Str::increment($values[$i], $i);
			$expected = $values[$i].'_'.$i;

			$this->assertEquals($expected, $output);
		}
	}

	/**
	 * Test for Str::lower()
	 *
	 * @test
	 */
	public function test_lower()
	{
		$output = Str::lower('HELLO WORLD');
		$expected = "hello world";

		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Str::upper()
	 *
	 * @test
	 */
	public function test_upper()
	{
		$output = Str::upper('hello world');
		$expected = "HELLO WORLD";

		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Str::lcfirst()
	 *
	 * @test
	 */
	public function test_lcfirst()
	{
		$output = Str::lcfirst('Hello World');
		$expected = "hello World";

		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Str::ucfirst()
	 *
	 * @test
	 */
	public function test_ucfirst()
	{
		$output = Str::ucfirst('hello world');
		$expected = "Hello world";

		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Str::ucwords()
	 *
	 * @test
	 */
	public function test_ucwords()
	{
		$output = Str::ucwords('hello world');
		$expected = "Hello World";

		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Str::random()
	 *
	 * @test
	 */
	public function test_random()
	{
		// testing length
		$output = Str::random('alnum', 34);
		$this->assertEquals(34, strlen($output));

		// testing alnum
		$output = Str::random('alnum', 15);
		$this->assertTrue(ctype_alnum($output));

		// testing numeric
		$output = Str::random('numeric', 20);
		$this->assertTrue(ctype_digit($output));

		// testing alpha
		$output = Str::random('alpha', 35);
		$this->assertTrue(ctype_alpha($output));

		// testing nozero
		$output = Str::random('nozero', 22);
		$this->assertFalse(strpos($output, '0'));
	}

	/**
	 * Test for Str::is_json()
	 *
	 * @test
	 */
	public function test_is_json()
	{
		$values = array('fuelphp','is' => array('awesome' => true));

		$string = json_encode($values);
		$this->assertTrue(Str::is_json($string));

		$string = serialize($values);
		$this->assertFalse(Str::is_json($string));
	}

	/**
	 * Test for Str::is_xml()
	 *
	 * @test
	 * @requires extension libxml
	 */
	public function test_is_xml()
	{
		$valid_xml = '<?xml version="1.0" encoding="UTF-8"?>
					<phpunit colors="true" stopOnFailure="false" bootstrap="bootstrap_phpunit.php">
						<php>
							<server name="doc_root" value="../../"/>
							<server name="app_path" value="fuel/app"/>
							<server name="core_path" value="fuel/core"/>
							<server name="package_path" value="fuel/packages"/>
						</php>
					</phpunit>';

		$invalid_xml = '<?xml version="1.0" encoding="UTF-8"?>
					<phpunit colors="true" stopOnFailure="false" bootstrap="bootstrap_phpunit.php">
						<php>
							<server name="doc_root" value="../../"/>
							<server name="app_path" value="fuel/app"/>
							<server name="core_path" value="fuel/core"/>
							<server name="package_path" value="fuel/packages"/>
						</
					</phpunit>';

		$this->assertTrue(Str::is_xml($valid_xml));
		$this->assertFalse(Str::is_xml($invalid_xml));
	}

	/**
	 * Test for Str::is_serialized()
	 *
	 * @test
	 */
	public function test_is_serialized()
	{
		$values = array('fuelphp','is' => array('awesome' => true));

		$string = json_encode($values);
		$this->assertFalse(Str::is_serialized($string));

		$string = serialize($values);
		$this->assertTrue(Str::is_serialized($string));
	}

	/**
	 * Test for Str::is_html()
	 *
	 * @test
	 */
	public function test_is_html()
	{
		$html = '<div class="row"><div class="span12"><strong>FuelPHP</strong> is a simple, flexible, <i>community<i> driven PHP 5.3 web framework based on the best ideas of other frameworks with a fresh start.</p>';
		$simple_string = strip_tags($html);

		$this->assertTrue(Str::is_html($html));
		$this->assertFalse(Str::is_html($simple_string));
	}

	/**
	 * Test for Str::starts_with()
	 *
	 * @test
	 */
	public function test_starts_with()
	{
		$string = 'HELLO WORLD';

		$output = Str::starts_with($string, 'HELLO');
		$this->assertTrue($output);

		$output = Str::starts_with($string, 'hello');
		$this->assertFalse($output);

		$output = Str::starts_with($string, 'hello', true);
		$this->assertTrue($output);
	}

	/**
	 * Test for Str::ends_with()
	 *
	 * @test
	 */
	public function test_ends_with()
	{
		$string = 'HELLO WORLD';

		$output = Str::ends_with($string, 'WORLD');
		$this->assertTrue($output);

		$output = Str::ends_with($string, 'world');
		$this->assertFalse($output);

		$output = Str::ends_with($string, 'world', true);
		$this->assertTrue($output);
	}

	/**
	 * Test for Str::alternator()
	 *
	 * @test
	 */
	public function test_alternator()
	{
		$alt = Str::alternator('one', 'two', 'three');

		$output = $alt();
		$expected = 'one';
		$this->assertEquals($output, $expected);

		$output = $alt(false);
		$expected = 'two';
		$this->assertEquals($output, $expected);

		$output = $alt();
		$expected = 'two';
		$this->assertEquals($output, $expected);

		$output = $alt();
		$expected = 'three';
		$this->assertEquals($output, $expected);

		$output = $alt();
		$expected = 'one';
		$this->assertEquals($output, $expected);
	}

}
