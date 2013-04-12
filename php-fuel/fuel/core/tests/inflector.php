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
 * Inflector class tests
 *
 * @group Core
 * @group Inflector
 */
class Test_Inflector extends TestCase
{

	public function ordinalize_provider()
	{
		return array(
			array(1, 'st'),
			array(21, 'st'),
			array(2, 'nd'),
			array(22, 'nd'),
			array(3, 'rd'),
			array(23, 'rd'),
			array(4, 'th'),
			array(24, 'th'),
			array(111, 'th'),
			array(112, 'th'),
			array(113, 'th'),
		);
	}

	/**
	 * Test for Inflector::ordinalize()
	 *
	 * @test
	 * @dataProvider ordinalize_provider
	 */
	public function test_ordinalize($number, $ending)
	{
		$this->assertEquals($number.$ending, Inflector::ordinalize($number));
	}

	/**
	 * Test for Inflector::ordinalize()
	 *
	 * @test
	 */
	public function test_ordinalize_of_string()
	{
		$this->assertEquals('Foo', Inflector::ordinalize('Foo'));
	}

	/**
	 * Test for Inflector::ascii()
	 *
	 * @test
	 */
	public function test_ascii()
	{
		$output = Inflector::ascii('Inglés');
		$expected = "Ingles";
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Inflector::camelize()
	 *
	 * @test
	 */
	public function test_camelize()
	{
		$output = Inflector::camelize('apples_and_oranges');
		$expected = 'ApplesAndOranges';
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Inflector::classify()
	 *
	 * @test
	 */
	public function test_classify()
	{
		$output = Inflector::classify('fuel_users');
		$expected = 'Fuel_User';
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Inflector::demodulize()
	 *
	 * @test
	 */
	public function test_demodulize()
	{
		$output = Inflector::demodulize('Uri::main()');
		$expected = 'main()';
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Inflector::denamespace()
	 *
	 * @test
	 */
	public function test_denamespace()
	{
		$this->assertEquals(Inflector::denamespace('Fuel\\SomeClass'), 'SomeClass');
		$this->assertEquals(Inflector::denamespace('\\SomeClass'), 'SomeClass');
		$this->assertEquals(Inflector::denamespace('SomeClass'), 'SomeClass');
		$this->assertEquals(Inflector::denamespace('SomeClass\\'), 'SomeClass');
	}

	/**
	 * Test for Inflector::foreign_key()
	 *
	 * @test
	 */
	public function test_foreign_key()
	{
		$output = Inflector::foreign_key('Inflector');
		$expected = 'inflector_id';
		$this->assertEquals($expected, $output);

		$output = Inflector::foreign_key('Inflector', false);
		$expected = 'inflectorid';
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Inflector::foreign_key()
	 *
	 * @test
	 */
	public function test_foreign_key_with_model_prefx()
	{
		$this->assertEquals('inflector_id', Inflector::foreign_key('Model_Inflector'));
	}

	/**
	 * Test for Inflector::friendly_title()
	 *
	 * @test
	 */
	public function test_friendly_title()
	{
		$output = Inflector::friendly_title('Fuel is a community driven PHP 5 web framework.');
		$expected = 'Fuel-is-a-community-driven-PHP-5-web-framework';
		$this->assertEquals($expected, $output);
	}

	public function test_friendly_title_sep()
	{
		$output = Inflector::friendly_title('Fuel is a community driven PHP 5 web framework.', '_');
		$expected = 'Fuel_is_a_community_driven_PHP_5_web_framework';
		$this->assertEquals($expected, $output);
	}

	public function test_friendly_title_lowercase()
	{
		$output = Inflector::friendly_title('Fuel is a community driven PHP 5 web framework.', '-', true);
		$expected = 'fuel-is-a-community-driven-php-5-web-framework';
		$this->assertEquals($expected, $output);
	}

	public function test_friendly_title_non_ascii()
	{
		$output = Inflector::friendly_title('وقود هو مجتمع مدفوعة إطار شبكة الإنترنت');
		$expected = '';
		$this->assertEquals($expected, $output);
	}

	public function test_friendly_title_allow_non_ascii()
	{
		$output = Inflector::friendly_title('وقود هو مجتمع مدفوعة إطار شبكة الإنترنت', '-', false, true);
		$expected = 'وقود-هو-مجتمع-مدفوعة-إطار-شبكة-الإنترنت';
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Inflector::humanize()
	 *
	 * @test
	 */
	public function test_humanize()
	{
		$output = Inflector::humanize('apples_and_oranges');
		$expected = 'Apples and oranges';
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Inflector::is_countable()
	 *
	 * @test
	 */
	public function test_is_countable()
	{
		$output = Inflector::is_countable('fish');
		$this->assertFalse($output);

		$output = Inflector::is_countable('apple');
		$this->assertTrue($output);
	}

	/**
	 * Test for Inflector::pluralize()
	 *
	 * @test
	 */
	public function test_pluralize()
	{
		$output = Inflector::pluralize('apple');
		$expected = "apples";
		$this->assertEquals($expected, $output);

		$output = Inflector::pluralize('apple', 1);
		$expected = "apple";
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Inflector::pluralize()
	 *
	 * @test
	 */
	public function test_pluralize_uncountable()
	{
		$this->assertEquals('equipment', Inflector::pluralize('equipment'));
	}

	/**
	 * Test for Inflector::singularize()
	 *
	 * @test
	 */
	public function test_singularize()
	{
		$output = Inflector::singularize('apples');
		$expected = "apple";
		$this->assertEquals($expected, $output);
	}

	/**
	 * Test for Inflector::singularize()
	 *
	 * @test
	 */
	public function test_singularize_uncountable()
	{
		$this->assertEquals('equipment', Inflector::singularize('equipment'));
	}

	public function tableize_provider()
	{
		return array(
			array('\\Model\\User', 'users'),
			array('\\Model\\Person', 'people'),
			array('\\Model\\Mouse', 'mice'),
			array('\\Model\\Ox', 'oxen'),
			array('\\Model\\Matrix', 'matrices'),
			array('Model_User', 'users'),
		);
	}

	/**
	 * Test for Inflector::tableize()
	 *
	 * @test
	 * @dataProvider tableize_provider
	 */
	public function test_tableize($class, $table)
	{
		$this->assertEquals(Inflector::tableize($class), $table);
	}

	public function get_namespace_provider()
	{
		return array(
			array('\\Model\\User', 'Model\\'),
			array('\\Fuel\\Core\\Inflector', 'Fuel\\Core\\'),
			array('Model_User', ''),
		);
	}

	/**
	 * Test for Inflector::get_namespace()
	 *
	 * @test
	 * @dataProvider get_namespace_provider
	 */
	public function test_get_namespace($class, $namespace)
	{
		$this->assertEquals(Inflector::get_namespace($class), $namespace);
	}

	/**
	 * Test for Inflector::underscore()
	 *
	 * @test
	 */
	public function test_underscore()
	{
		$output = Inflector::underscore('ApplesAndOranges');
		$expected = "apples_and_oranges";
		$this->assertEquals($expected, $output);
	}
}

