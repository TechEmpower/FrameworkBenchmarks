<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Unit tests for response class
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.response
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_ResponseTest extends Unittest_TestCase
{
	/**
	 * Provider for test_body
	 *
	 * @return array
	 */
	public function provider_body()
	{
		$view = $this->getMock('View');
		$view->expects($this->any())
			->method('__toString')
			->will($this->returnValue('foo'));

		return array(
			array('unit test', 'unit test'),
			array($view, 'foo'),
		);
	}

	/**
	 * Tests that we can set and read a body of a response
	 * 
	 * @test
	 * @dataProvider provider_body
	 *
	 * @return null
	 */
	public function test_body($source, $expected)
	{
		$response = new Response;
		$response->body($source);
		$this->assertSame($response->body(), $expected);

		$response = (string) $response;
		$this->assertSame($response, $expected);
	}

	/**
	 * Provides data for test_body_string_zero()
	 *
	 * @return array
	 */
	public function provider_body_string_zero()
	{
		return array(
			array('0', '0'),
			array("0", '0'),
			array(0, '0')
		);
	}

	/**
	 * Test that Response::body() handles numerics correctly
	 *
	 * @test
	 * @dataProvider provider_body_string_zero
	 * @param string $string 
	 * @param string $expected 
	 * @return void
	 */
	public function test_body_string_zero($string, $expected)
	{
		$response = new Response;
		$response->body($string);

		$this->assertSame($expected, $response->body());
	}

	/**
	 * provider for test_cookie_set()
	 *
	 * @return array
	 */
	public function provider_cookie_set()
	{
		return array(
			array(
				'test1',
				'foo',
				array(
					'test1' => array(
						'value' => 'foo',
						'expiration' => Cookie::$expiration
					),
				)
			),
			array(
				array(
					'test2' => 'stfu',
					'test3' => array(
						'value' => 'snafu',
						'expiration' => 123456789
					)
				),
				NULL,
				array(
					'test2' => array(
						'value' => 'stfu',
						'expiration' => Cookie::$expiration
					),
					'test3' => array(
						'value' => 'snafu',
						'expiration' => 123456789
					)
				)
			)
		);
	}

	/**
	 * Tests the Response::cookie() method, ensures
	 * correct values are set, including defaults
	 *
	 * @test
	 * @dataProvider provider_cookie_set
	 * @param string $key 
	 * @param string $value 
	 * @param string $expected 
	 * @return void
	 */
	public function test_cookie_set($key, $value, $expected)
	{
		// Setup the Response and apply cookie
		$response = new Response;
		$response->cookie($key, $value);

		foreach ($expected as $_key => $_value)
		{
			$cookie = $response->cookie($_key);

			$this->assertSame($_value['value'], $cookie['value']);
			$this->assertSame($_value['expiration'], $cookie['expiration']);
		}
	}

	/**
	 * Tests the Response::cookie() get functionality
	 *
	 * @return void
	 */
	public function test_cookie_get()
	{
		$response = new Response;

		// Test for empty cookies
		$this->assertSame(array(), $response->cookie());

		// Test for no specific cookie
		$this->assertNull($response->cookie('foobar'));

		$response->cookie('foo', 'bar');
		$cookie = $response->cookie('foo');

		$this->assertSame('bar', $cookie['value']);
		$this->assertSame(Cookie::$expiration, $cookie['expiration']);
	}

	/**
	 * Tests that the headers are not sent by PHP in CLI mode
	 *
	 * @return void
	 */
	public function test_send_headers_cli()
	{
		if (headers_sent())
		{
			$this->markTestSkipped('Cannot test this feature as headers have already been sent!');
		}

		$content_type = 'application/json';
		$response = new Response;
		$response->headers('content-type', $content_type)
			->send_headers();

		$this->assertFalse(headers_sent());

	}

	/**
	 * Test the content type is sent when set
	 * 
	 * @test
	 */
	public function test_content_type_when_set()
	{
		$content_type = 'application/json';
		$response = new Response;
		$response->headers('content-type', $content_type);
		$headers  = $response->send_headers()->headers();
		$this->assertSame($content_type, (string) $headers['content-type']);
	}
}