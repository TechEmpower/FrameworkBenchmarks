<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Tests Kohana i18n class
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.i18n
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @author     Jeremy Bush <contractfrombelow@gmail.com>
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_I18nTest extends Unittest_TestCase {

	/**
	 * Default values for the environment, see setEnvironment
	 * @var array
	 */
	// @codingStandardsIgnoreStart
	protected $environmentDefault =	array(
		'I18n::$lang' => 'en-us',
	);
	// @codingStandardsIgnoreEnd

	/**
	 * Provides test data for test_lang()
	 *
	 * @return array
	 */
	public function provider_lang()
	{
		return array(
			// $input, $expected_result
			array(NULL, 'en-us'),
			array('es-es', 'es-es'),
		);
	}

	/**
	 * Tests I18n::lang()
	 *
	 * @test
	 * @dataProvider provider_lang
	 * @param  boolean  $input     Input for I18n::lang
	 * @param  boolean  $expected  Output for I18n::lang
	 */
	public function test_lang($input, $expected_result)
	{
		$this->assertSame($expected_result, I18n::lang($input));
		$this->assertSame($expected_result, I18n::lang());
	}

	/**
	 * Provides test data for test_get()
	 * 
	 * @return array
	 */
	public function provider_get()
	{
		return array(
			// $value, $result
			array('en-us', 'Hello, world!', 'Hello, world!'),
			array('es-es', 'Hello, world!', '¡Hola, mundo!'),
			array('fr-fr', 'Hello, world!', 'Bonjour, monde!'),
		);
	}

	/**
	 * Tests i18n::get()
	 *
	 * @test
	 * @dataProvider provider_get
	 * @param boolean $input  Input for File::mime
	 * @param boolean $expected Output for File::mime
	 */
	public function test_get($lang, $input, $expected)
	{
		I18n::lang($lang);
		$this->assertSame($expected, I18n::get($input));

		// Test immediate translation, issue #3085
		I18n::lang('en-us');
		$this->assertSame($expected, I18n::get($input, $lang));
	}

}
