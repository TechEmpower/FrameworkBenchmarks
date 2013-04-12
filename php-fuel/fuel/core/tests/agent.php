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
 * Agent class tests
 *
 * @group Core
 * @group Agent
 */
class Test_Agent extends TestCase
{

	/**
	 * need to setup a fake browser environment
	 */
	protected function setUp()
	{
		$_SERVER['HTTP_ACCEPT_LANGUAGE'] = 'en-us,en;q=0.8,nl-be;q=0.5,nl;q=0.3';
		$_SERVER['HTTP_ACCEPT_CHARSET'] = 'UTF-8,ISO-8859-1,*';
		$_SERVER['HTTP_USER_AGENT'] = 'Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.2.16) Gecko/20110322 Fedora/3.6.16-1.fc14 Firefox/3.6.16';
	}

	/**
	 * Tests Agent::browser()
	 *
	 * @test
	 */
	public function test_browser()
 	{
		$expected = "Firefox";
		$output = Agent::browser();
		$this->assertEquals($expected, $output);
	}

	/**
	 * Tests Agent::platform()
	 *
	 * @test
	 */
	public function test_platform()
 	{
		$expected = "Linux";
		$output = Agent::platform();
		$this->assertEquals($expected, $output);
	}

	/**
	 * Tests Agent::version()
	 *
	 * @test
	 */
	public function test_version()
 	{
		$expected = 3.6;
		$output = Agent::version();
		$this->assertInternalType('float', $output);
		$this->assertEquals($expected, $output);
	}

	public function property_provider()
 	{
		return array(
			array(
				'Browser','Firefox',
			),
			array(
				'Version',3.6,
			),
			array(
				'MajorVer',3,
			),
			array(
				'MinorVer',6,
			),
			array(
				'Platform','Linux',
			),
			array(
				'Alpha',false,
			),
			array(
				'Beta',false,
			),
			array(
				'Win16',false,
			),
			array(
				'Win32',false,
			),
			array(
				'Win64',false,
			),
			array(
				'Frames',true,
			),
			array(
				'IFrames',true,
			),
			array(
				'Tables',true,
			),
			array(
				'Cookies',true,
			),
			array(
				'BackgroundSounds',false,
			),
			array(
				'JavaScript',true,
			),
			array(
				'VBScript',false,
			),
			array(
				'JavaApplets',true,
			),
			array(
				'ActiveXControls',false,
			),
			array(
				'isBanned',false,
			),
			array(
				'isMobile',false,
			),
			array(
				'isSyndicationReader',false,
			),
			array(
				'Crawler',false,
			),
			array(
				'CssVersion',3,
			),
			array(
				'AolVersion',0,
			),
		);
	}

	/**
	 * Tests Agent::property()
	 *
	 * @test
	 * @dataProvider property_provider
	 */
	public function test_property($property, $expected)
 	{
		$output = Agent::property($property);
		$this->assertEquals($expected, $output);
	}

	/**
	 * Tests Agent::is_robot()
	 *
	 * @test
	 */
	public function test_is_robot()
 	{
		$output = Agent::is_robot();
		$this->assertFalse($output);
	}

	/**
	 * Tests Agent::is_mobiledevice()
	 *
	 * @test
	 */
	public function test_is_mobiledevice()
 	{
		$output = Agent::is_mobiledevice();
		$this->assertFalse($output);
	}

	/**
	 * Tests Agent::languages()
	 *
	 * @test
	 */
	public function test_languages()
 	{
		$expected = array("en-us", "en", "nl-be", "nl");
		$output = Agent::languages();
		$this->assertEquals($expected, $output);
	}

	/**
	 * Tests Agent::accepts_language()
	 *
	 * @test
	 */
	public function test_accepts_language_success()
 	{
		$output = Agent::accepts_language('en-us');
		$this->assertTrue($output);
	}

	public function test_accepts_language_fail()
 	{
		$output = Agent::accepts_language('pt-br');
		$this->assertFalse($output);
	}

	/**
	 * Tests Agent::charsets()
	 *
	 * @test
	 */
	public function test_charsets()
 	{
		$expected = array("utf-8", "iso-8859-1", "*");
		$output = Agent::charsets();
		$this->assertEquals($expected, $output);
	}

	/**
	 * Tests Agent::accepts_charset()
	 *
	 * @test
	 */
	public function test_accepts_charset_success()
 	{
		$output = Agent::accepts_charset('utf-8');
		$this->assertTrue($output);
	}

	public function test_accepts_charset_fail()
 	{
		$output = Agent::accepts_charset('cp2');
		$this->assertFalse($output);
	}
}
