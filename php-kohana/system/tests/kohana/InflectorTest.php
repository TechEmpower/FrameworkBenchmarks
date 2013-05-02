<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Tests Kohana inflector class
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.inflector
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @author     Jeremy Bush <contractfrombelow@gmail.com>
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_InflectorTest extends Unittest_TestCase
{
	/**
	 * Provides test data for test_lang()
	 * 
	 * @return array
	 */
	public function provider_uncountable()
	{
		return array(
			// $value, $result
			array('fish', TRUE),
			array('cat', FALSE),
			array('deer', TRUE),
			array('bison', TRUE),
			array('friend', FALSE),
		);
	}

	/**
	 * Tests Inflector::uncountable
	 *
	 * @test
	 * @dataProvider provider_uncountable
	 * @param boolean $input  Input for File::mime
	 * @param boolean $expected Output for File::mime
	 */
	public function test_uncountable($input, $expected)
	{
		$this->assertSame($expected, Inflector::uncountable($input));
	}

	/**
	 * Provides test data for test_lang()
	 * 
	 * @return array
	 */
	public function provider_singular()
	{
		return array(
			// $value, $result
			array('fish', NULL, 'fish'),
			array('cats', NULL, 'cat'),
			array('cats', 2, 'cats'),
			array('cats', '2', 'cats'),
			array('children', NULL, 'child'),
			array('meters', 0.6, 'meters'),
			array('meters', 1.6, 'meters'),
			array('meters', 1.0, 'meter'),
			array('status', NULL, 'status'),
			array('statuses', NULL, 'status'),
			array('heroes', NULL, 'hero'),
		);
	}

	/**
	 * Tests Inflector::singular
	 *
	 * @test
	 * @dataProvider provider_singular
	 * @param boolean $input  Input for File::mime
	 * @param boolean $expected Output for File::mime
	 */
	public function test_singular($input, $count, $expected)
	{
		$this->assertSame($expected, Inflector::singular($input, $count));
	}

	/**
	 * Provides test data for test_lang()
	 * 
	 * @return array
	 */
	public function provider_plural()
	{
		return array(
			// $value, $result
			array('fish', NULL, 'fish'),
			array('cat', NULL, 'cats'),
			array('cats', 1, 'cats'),
			array('cats', '1', 'cats'),
			array('movie', NULL, 'movies'),
			array('meter', 0.6, 'meters'),
			array('meter', 1.6, 'meters'),
			array('meter', 1.0, 'meter'),
			array('hero', NULL, 'heroes'),
			array('Dog', NULL, 'Dogs'), // Titlecase
			array('DOG', NULL, 'DOGS'), // Uppercase
		);
	}

	/**
	 * Tests Inflector::plural
	 *
	 * @test
	 * @dataProvider provider_plural
	 * @param boolean $input  Input for File::mime
	 * @param boolean $expected Output for File::mime
	 */
	public function test_plural($input, $count, $expected)
	{
		$this->assertSame($expected, Inflector::plural($input, $count));
	}

	/**
	 * Provides test data for test_camelize()
	 * 
	 * @return array
	 */
	public function provider_camelize()
	{
		return array(
			// $value, $result
			array('mother cat', 'camelize', 'motherCat'),
			array('kittens in bed', 'camelize', 'kittensInBed'),
			array('mother cat', 'underscore', 'mother_cat'),
			array('kittens in bed', 'underscore', 'kittens_in_bed'),
			array('kittens-are-cats', 'humanize', 'kittens are cats'),
			array('dogs_as_well', 'humanize', 'dogs as well'),
		);
	}

	/**
	 * Tests Inflector::camelize
	 *
	 * @test
	 * @dataProvider provider_camelize
	 * @param boolean $input  Input for File::mime
	 * @param boolean $expected Output for File::mime
	 */
	public function test_camelize($input, $method, $expected)
	{
		$this->assertSame($expected, Inflector::$method($input));
	}

	/**
	 * Provides data for test_decamelize()
	 *
	 * @return array
	 */
	public function provider_decamelize()
	{
		return array(
			array('getText', '_', 'get_text'),
			array('getJSON', '_', 'get_json'),
			array('getLongText', '_', 'get_long_text'),
			array('getI18N', '_', 'get_i18n'),
			array('getL10n', '_', 'get_l10n'),
			array('getTe5t1ng', '_', 'get_te5t1ng'),
			array('OpenFile', '_', 'open_file'),
			array('CloseIoSocket', '_', 'close_io_socket'),
			array('fooBar', ' ', 'foo bar'),
			array('camelCase', '+', 'camel+case'),
		);
	}

	/**
	 * Tests Inflector::decamelize()
	 *
	 * @test
	 * @dataProvider provider_decamelize
	 * @param string $input Camelized string
	 * @param string $glue Glue
	 * @param string $expected Expected string
	 */
	public function test_decamelize($input, $glue, $expected)
	{
		$this->assertSame($expected, Inflector::decamelize($input, $glue));
	}
}
