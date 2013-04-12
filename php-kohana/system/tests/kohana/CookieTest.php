<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Tests the cookie class
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.cookie
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @author     Jeremy Bush <contractfrombelow@gmail.com>
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_CookieTest extends Unittest_TestCase
{

	protected $_default_salt = 'AdaoidadnA£ASDNadnaoiwdnawd';
	/**
	 * Sets up the environment
	 */
	// @codingStandardsIgnoreStart
	public function setUp()
	// @codingStandardsIgnoreEnd
	{
		parent::setUp();

		Cookie::$salt = $this->_default_salt;
	}

	/**
	 * Tears down the environment
	 */
	// @codingStandardsIgnoreStart
	public function tearDown()
	// @codingStandardsIgnoreEnd
	{
		parent::tearDown();

		Cookie::$salt = NULL;
	}

	/**
	 * Provides test data for test_set()
	 *
	 * @return array
	 */
	public function provider_set()
	{
		return array(
			array('foo', 'bar', NULL, TRUE),
			array('foo', 'bar', 10, TRUE),
		);
	}

	/**
	 * Tests cookie::set()
	 *
	 * @test
	 * @dataProvider provider_set
	 * @covers cookie::set
	 * @param mixed   $key      key to use
	 * @param mixed   $value    value to set
	 * @param mixed   $exp      exp to set
	 * @param boolean $expected Output for cookie::set()
	 */
	public function test_set($key, $value, $exp, $expected)
	{
		if (headers_sent()) {
			$this->markTestSkipped('Cannot test setting cookies as headers have already been sent');
		}

		$this->assertSame($expected, cookie::set($key, $value, $exp));
	}

	/**
	 * Provides test data for test_get()
	 *
	 * @return array
	 */
	public function provider_get()
	{
		// setUp is called after the provider so we need to specify a
		// salt here in order to use it in the provider
		Cookie::$salt = $this->_default_salt;

		return array(
			array('foo', Cookie::salt('foo', 'bar').'~bar', 'bar'),
			array('bar', Cookie::salt('foo', 'bar').'~bar', NULL),
			array(NULL, Cookie::salt('foo', 'bar').'~bar', NULL),
		);
	}

	/**
	 * Tests cookie::set()
	 *
	 * @test
	 * @dataProvider provider_get
	 * @covers cookie::get
	 * @param mixed   $key      key to use
	 * @param mixed   $value    value to set
	 * @param boolean $expected Output for cookie::get()
	 */
	public function test_get($key, $value, $expected)
	{
		if (headers_sent()) {
			$this->markTestSkipped('Cannot test setting cookies as headers have already been sent');
		}

		// Force $_COOKIE
		if ($key !== NULL)
		{
			$_COOKIE[$key] = $value;
		}

		$this->assertSame($expected, cookie::get($key));
	}

	/**
	 * Provides test data for test_delete()
	 *
	 * @return array
	 */
	public function provider_delete()
	{
		return array(
			array('foo', TRUE),
		);
	}

	/**
	 * Tests cookie::delete()
	 *
	 * @test
	 * @dataProvider provider_delete
	 * @covers cookie::delete
	 * @param mixed   $key      key to use
	 * @param boolean $expected Output for cookie::delete()
	 */
	public function test_delete($key, $expected)
	{
		if (headers_sent()) {
			$this->markTestSkipped('Cannot test setting cookies as headers have already been sent');
		}

		$this->assertSame($expected, cookie::delete($key));
	}

	/**
	 * Provides test data for test_salt()
	 *
	 * @return array
	 */
	public function provider_salt()
	{
		return array(
			array('foo', 'bar', 'b5773a6255d1deefc23f9f69bcc40fdc998e5802'),
		);
	}

	/**
	 * Tests cookie::salt()
	 *
	 * @test
	 * @dataProvider provider_salt
	 * @covers cookie::salt
	 * @param mixed   $key      key to use
	 * @param mixed   $value    value to salt with
	 * @param boolean $expected Output for cookie::delete()
	 */
	public function test_salt($key, $value, $expected)
	{
		$this->assertSame($expected, cookie::salt($key, $value));
	}
}
