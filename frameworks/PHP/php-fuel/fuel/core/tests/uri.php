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
 * Html class tests
 *
 * @group Core
 * @group Uri
 */
class Test_Uri extends TestCase
{

	/**
	 * Tests Uri::create()
	 *
	 * @test
	 */
	public function test_create()
	{
		Config::set('url_suffix', '');

		$prefix = Uri::create('');

		Config::set('index_file', 'index.php');
		$output = Uri::create('controller/method');
		$expected = $prefix."index.php/controller/method";
		$this->assertEquals($expected, $output);

		Config::set('index_file', '');

		$output = Uri::create('controller/method');
		$expected = $prefix."controller/method";
		$this->assertEquals($expected, $output);

		$output = Uri::create('controller/:some', array('some' => 'thing', 'and' => 'more'), array('what' => ':and'));
		$expected = $prefix."controller/thing?what=more";
		$this->assertEquals($expected, $output);

		Config::set('url_suffix', '.html');

		$output = Uri::create('controller/method');
		$expected = $prefix."controller/method.html";
		$this->assertEquals($expected, $output);

		$output = Uri::create('controller/:some', array('some' => 'thing', 'and' => 'more'), array('what' => ':and'));
		$expected = $prefix."controller/thing.html?what=more";
		$this->assertEquals($expected, $output);

		$output = Uri::create('http://example.com/controller/:some', array('some' => 'thing', 'and' => 'more'), array('what' => ':and'));
		$expected = "http://example.com/controller/thing.html?what=more";
		$this->assertEquals($expected, $output);

		$output = Uri::create('http://example.com/controller/:some', array('some' => 'thing', 'and' => 'more'), array('what' => ':and'), true);
		$expected = "https://example.com/controller/thing.html?what=more";
		$this->assertEquals($expected, $output);

		$output = Uri::create('https://example.com/controller/:some', array('some' => 'thing', 'and' => 'more'), array('what' => ':and'), false);
		$expected = "http://example.com/controller/thing.html?what=more";
		$this->assertEquals($expected, $output);

	}

	/**
	 * Tests Uri::base()
	 *
	 * @test
	 */
	public function test_base()
	{
		Config::set('base_url', null);
		Config::set('index_file', false);

		$output = Uri::base();
		$expected = null;
		$this->assertEquals($expected, $output);

		Config::set('base_url', 'http://example.com/');
		Config::set('index_file', 'index.php');

		$output = Uri::base();
		$expected = 'http://example.com/index.php/';
		$this->assertEquals($expected, $output);

		$output = Uri::base(false);
		$expected = 'http://example.com/';
		$this->assertEquals($expected, $output);
	}

	/**
	 * Tests Uri::current()
	 *
	 * @test
	 */
	public function test_current()
	{
		$output = Uri::current();
		$expected = Uri::create();
		$this->assertEquals($expected, $output);
	}

}


