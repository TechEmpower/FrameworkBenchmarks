<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Tests HTTP
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.http
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_HTTPTest extends Unittest_TestCase {

	/**
	 * Defaults for this test
	 * @var array
	 */
	// @codingStandardsIgnoreStart
	protected $environmentDefault = array(
		'Kohana::$base_url'    => '/kohana/',
		'Kohana::$index_file'  => 'index.php',
		'HTTP_HOST'	           => 'www.example.com',
	);
	// @codingStandardsIgnoreEnd

	/**
	 * Provides test data for test_attributes()
	 *
	 * @return array
	 */
	public function provider_redirect()
	{
		return array(
			array(
				'http://www.example.org/',
				301,
				'HTTP_Exception_301',
				'http://www.example.org/'
			),
			array(
				'/page_one',
				302,
				'HTTP_Exception_302',
				'http://www.example.com/kohana/index.php/page_one'
			),
			array(
				'page_two',
				303,
				'HTTP_Exception_303',
				'http://www.example.com/kohana/index.php/page_two'
			),
		);
	}

	/**
	 * Tests HTTP::redirect()
	 *
	 * @test
	 * @dataProvider provider_redirect
	 * @param array  $location            Location to redirect to
	 * @param array  $code                HTTP Code to use for the redirect
	 * @param string $expected_exception  Expected exception
	 * @param string $expected_location   Expected exception
	 */
	public function test_redirect($location, $code, $expected_exception, $expected_location)
	{
		try
		{
			HTTP::redirect($location, $code);
		}
		catch (HTTP_Exception_Redirect $e)
		{
			$response = $e->get_response();

			$this->assertInstanceOf($expected_exception, $e);
			$this->assertEquals($expected_location, $response->headers('Location'));
			
			return;
		}

		$this->fail('HTTP_Exception_Redirect not thrown');
	}
}
