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
 * Security class tests
 *
 * @group Core
 * @group Security
 */
class Test_Security extends TestCase
{
	/**
	* Tests Security::htmlentities()
	*
	* @test
	*/
	public function test_htmlentities_doublequote_and_ampersand()
	{
		$output = Security::htmlentities('"H&M"');
		$expected = '&quot;H&amp;M&quot;';
		$this->assertEquals($expected, $output);
	}

	/**
	* Tests Security::htmlentities()
	*
	* @test
	*/
	public function test_htmlentities_singlequote()
	{
		$output = Security::htmlentities("'");
		$expected = '&#039;';
		$this->assertEquals($expected, $output);
	}

	/**
	* Tests Security::htmlentities()
	*
	* @test
	*/
	public function test_htmlentities_charactor_references_no_double_encode()
	{
		$output = Security::htmlentities('You must write & as &amp;');
		$expected = 'You must write &amp; as &amp;';
		$this->assertEquals($expected, $output);
	}

	/**
	* Tests Security::htmlentities()
	*
	* @test
	*/
	public function test_htmlentities_charactor_references_double_encode()
	{
		$config = \Config::get('security.htmlentities_double_encode');
		\Config::set('security.htmlentities_double_encode', true);

		$output = Security::htmlentities('You must write & as &amp;');
		$expected = 'You must write &amp; as &amp;amp;';
		$this->assertEquals($expected, $output);

		\Config::set('security.htmlentities_double_encode', $config);
	}

	/**
	* Tests Security::htmlentities()
	*
	* @test
	*/
	public function test_htmlentities_double_encode()
	{
		$output = Security::htmlentities('"H&M"');
		$output = Security::htmlentities($output);
		$expected = '&quot;H&amp;M&quot;';
		$this->assertEquals($expected, $output);
	}
}
