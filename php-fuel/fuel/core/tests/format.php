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
 * Format class tests
 *
 * @group Core
 * @group Format
 */
class Test_Format extends TestCase
{

	public static function array_provider()
	{
		return array(
			array(
				array(
					array('field1' => 'Value 1', 'field2' => 35, 'field3' => 123123),
					array('field1' => 'Value 1', 'field2' => "Value\nline 2", 'field3' => 'Value 3'),
				),
				'"field1","field2","field3"
"Value 1","35","123123"
"Value 1","Value
line 2","Value 3"',
			),
		);
	}

	/**
	 * Test for Format::forge($foo, 'csv')->to_array()
	 *
	 * @test
	 * @dataProvider array_provider
	 */
	public function test_from_csv($array, $csv)
	{
		$this->assertEquals($array, Format::forge($csv, 'csv')->to_array());

	}

	/**
	 * Test for Format::forge($foo)->to_csv()
	 *
	 * @test
	 * @dataProvider array_provider
	 */
	public function test_to_csv($array, $csv)
	{
		$this->assertEquals($csv, Format::forge($array)->to_csv());
	}

	/**
	 * Test for Format::forge($foo)->_from_xml()
	 *
	 * @test
	 */
	public function test__from_xml()
	{
		$xml = '<?xml version="1.0" encoding="UTF-8"?>

<phpunit colors="true" stopOnFailure="false" bootstrap="bootstrap_phpunit.php">
	<php>
		<server name="doc_root" value="../../"/>
		<server name="app_path" value="fuel/app"/>
		<server name="core_path" value="fuel/core"/>
		<server name="package_path" value="fuel/packages"/>
	</php>
	<testsuites>
		<testsuite name="core">
			<directory suffix=".php">../core/tests</directory>
		</testsuite>
		<testsuite name="packages">
			<directory suffix=".php">../packages/*/tests</directory>
		</testsuite>
		<testsuite name="app">
			<directory suffix=".php">../app/tests</directory>
		</testsuite>
	</testsuites>
</phpunit>';

		$expected = array (
			'@attributes' => array (
				'colors' => 'true',
				'stopOnFailure' => 'false',
				'bootstrap' => 'bootstrap_phpunit.php',
			),
			'php' => array (
				'server' => array (
					0 => array (
						'@attributes' => array (
							'name' => 'doc_root',
							'value' => '../../',
						),
					),
					1 => array (
						'@attributes' => array (
							'name' => 'app_path',
							'value' => 'fuel/app',
						),
					),
					2 => array (
						'@attributes' => array (
							'name' => 'core_path',
							'value' => 'fuel/core',
						),
					),
					3 => array (
						'@attributes' => array (
							'name' => 'package_path',
							'value' => 'fuel/packages',
						),
					),
				),
			),
			'testsuites' => array (
				'testsuite' => array (
					0 => array (
						'@attributes' => array (
							'name' => 'core',
						),
						'directory' => '../core/tests',
					),
					1 => array (
						'@attributes' =>
						array (
							'name' => 'packages',
						),
						'directory' => '../packages/*/tests',
					),
					2 => array (
						'@attributes' =>
						array (
							'name' => 'app',
						),
						'directory' => '../app/tests',
					),
				),
			),
		);

		$this->assertEquals(Format::forge($expected)->to_php(), Format::forge($xml, 'xml')->to_php());
	}

	function test_to_array_empty()
	{
		$array = null;
		$expected = array();
		$this->assertEquals($expected, Format::forge($array)->to_array());
	}
}
