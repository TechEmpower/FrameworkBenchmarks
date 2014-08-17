<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Tests Kohana Core
 *
 * @TODO Use a virtual filesystem (see phpunit doc on mocking fs) for find_file etc.
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.core
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @author     Jeremy Bush <contractfrombelow@gmail.com>
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_CoreTest extends Unittest_TestCase
{

	/**
	 * Provides test data for test_sanitize()
	 *
	 * @return array
	 */
	public function provider_sanitize()
	{
		return array(
			// $value, $result
			array('foo', 'foo'),
			array("foo\r\nbar", "foo\nbar"),
			array("foo\rbar", "foo\nbar"),
			array("Is your name O\'reilly?", "Is your name O'reilly?")
		);
	}

	/**
	 * Tests Kohana::santize()
	 *
	 * @test
	 * @dataProvider provider_sanitize
	 * @covers Kohana::sanitize
	 * @param boolean $value  Input for Kohana::sanitize
	 * @param boolean $result Output for Kohana::sanitize
	 */
	public function test_sanitize($value, $result)
	{
		$this->setEnvironment(array('Kohana::$magic_quotes' => TRUE));

		$this->assertSame($result, Kohana::sanitize($value));
	}

	/**
	 * Passing FALSE for the file extension should prevent appending any extension.
	 * See issue #3214
	 *
	 * @test
	 * @covers  Kohana::find_file
	 */
	public function test_find_file_no_extension()
	{
		// EXT is manually appened to the _file name_, not passed as the extension
		$path = Kohana::find_file('classes', $file = 'Kohana/Core'.EXT, FALSE);

		$this->assertInternalType('string', $path);

		$this->assertStringEndsWith($file, $path);
	}

	/**
	 * If a file can't be found then find_file() should return FALSE if
	 * only a single file was requested, or an empty array if multiple files
	 * (i.e. configuration files) were requested
	 *
	 * @test
	 * @covers Kohana::find_file
	 */
	public function test_find_file_returns_false_or_array_on_failure()
	{
		$this->assertFalse(Kohana::find_file('configy', 'zebra'));

		$this->assertSame(array(), Kohana::find_file('configy', 'zebra', NULL, TRUE));
	}

	/**
	 * Kohana::list_files() should return an array on success and an empty array on failure
	 *
	 * @test
	 * @covers Kohana::list_files
	 */
	public function test_list_files_returns_array_on_success_and_failure()
	{
		$files = Kohana::list_files('config');

		$this->assertInternalType('array', $files);
		$this->assertGreaterThan(3, count($files));

		$this->assertSame(array(), Kohana::list_files('geshmuck'));
	}

	/**
	 * Tests Kohana::globals()
	 *
	 * @test
	 * @covers Kohana::globals
	 */
	public function test_globals_removes_user_def_globals()
	{
		$GLOBALS = array('hackers' => 'foobar','name' => array('','',''), '_POST' => array());

		Kohana::globals();

		$this->assertEquals(array('_POST' => array()), $GLOBALS);
	}

	/**
	 * Provides test data for testCache()
	 *
	 * @return array
	 */
	public function provider_cache()
	{
		return array(
			// $value, $result
			array('foo', 'hello, world', 10),
			array('bar', NULL, 10),
			array('bar', NULL, -10),
		);
	}

	/**
	 * Tests Kohana::cache()
	 *
	 * @test
	 * @dataProvider provider_cache
	 * @covers Kohana::cache
	 * @param boolean $key      Key to cache/get for Kohana::cache
	 * @param boolean $value    Output from Kohana::cache
	 * @param boolean $lifetime Lifetime for Kohana::cache
	 */
	public function test_cache($key, $value, $lifetime)
	{
		Kohana::cache($key, $value, $lifetime);
		$this->assertEquals($value, Kohana::cache($key));
	}

	/**
	 * Provides test data for test_message()
	 *
	 * @return array
	 */
	public function provider_message()
	{
		return array(
			// $value, $result
			array(':field must not be empty', 'validation', 'not_empty'),
			array(
				array(
					'alpha'         => ':field must contain only letters',
					'alpha_dash'    => ':field must contain only numbers, letters and dashes',
					'alpha_numeric' => ':field must contain only letters and numbers',
					'color'         => ':field must be a color',
					'credit_card'   => ':field must be a credit card number',
					'date'          => ':field must be a date',
					'decimal'       => ':field must be a decimal with :param2 places',
					'digit'         => ':field must be a digit',
					'email'         => ':field must be a email address',
					'email_domain'  => ':field must contain a valid email domain',
					'equals'        => ':field must equal :param2',
					'exact_length'  => ':field must be exactly :param2 characters long',
					'in_array'      => ':field must be one of the available options',
					'ip'            => ':field must be an ip address',
					'matches'       => ':field must be the same as :param2',
					'min_length'    => ':field must be at least :param2 characters long',
					'max_length'    => ':field must not exceed :param2 characters long',
					'not_empty'     => ':field must not be empty',
					'numeric'       => ':field must be numeric',
					'phone'         => ':field must be a phone number',
					'range'         => ':field must be within the range of :param2 to :param3',
					'regex'         => ':field does not match the required format',
					'url'           => ':field must be a url',
				),
				'validation', NULL,
			),
		);
	}

	/**
	 * Tests Kohana::message()
	 *
	 * @test
	 * @dataProvider provider_message
	 * @covers Kohana::message
	 * @param boolean $expected Output for Kohana::message
	 * @param boolean $file     File to look in for Kohana::message
	 * @param boolean $key      Key for Kohana::message
	 */
	public function test_message($expected, $file, $key)
	{
		$this->markTestSkipped('This test is incredibly fragile and needs to be re-done');
		$this->assertEquals($expected, Kohana::message($file, $key));
	}

	/**
	 * Provides test data for test_error_handler()
	 *
	 * @return array
	 */
	public function provider_error_handler()
	{
		return array(
			array(1, 'Foobar', 'foobar.php', __LINE__),
		);
	}

	/**
	 * Tests Kohana::error_handler()
	 *
	 * @test
	 * @dataProvider provider_error_handler
	 * @covers Kohana::error_handler
	 * @param boolean $code  Input for Kohana::sanitize
	 * @param boolean $error  Input for Kohana::sanitize
	 * @param boolean $file  Input for Kohana::sanitize
	 * @param boolean $line Output for Kohana::sanitize
	 */
	public function test_error_handler($code, $error, $file, $line)
	{
		$error_level = error_reporting();
		error_reporting(E_ALL);
		try
		{
			Kohana::error_handler($code, $error, $file, $line);
		}
		catch (Exception $e)
		{
			$this->assertEquals($code, $e->getCode());
			$this->assertEquals($error, $e->getMessage());
		}
		error_reporting($error_level);
	}

	/**
	 * Provides test data for test_modules_sets_and_returns_valid_modules()
	 *
	 * @return array
	 */
	public function provider_modules_detects_invalid_modules()
	{
		return array(
			array(array('unittest' => MODPATH.'fo0bar')),
			array(array('unittest' => MODPATH.'unittest', 'fo0bar' => MODPATH.'fo0bar')),
		);
	}

	/**
	 * Tests Kohana::modules()
	 *
	 * @test
	 * @dataProvider provider_modules_detects_invalid_modules
	 * @expectedException Kohana_Exception
	 * @param boolean $source   Input for Kohana::modules
	 *
	 */
	public function test_modules_detects_invalid_modules($source)
	{
		$modules = Kohana::modules();

		try
		{
			Kohana::modules($source);
		}
		catch(Exception $e)
		{
			// Restore modules
			Kohana::modules($modules);

			throw $e;
		}

		// Restore modules
		Kohana::modules($modules);
	}

	/**
	 * Provides test data for test_modules_sets_and_returns_valid_modules()
	 *
	 * @return array
	 */
	public function provider_modules_sets_and_returns_valid_modules()
	{
		return array(
			array(array(), array()),
			array(array('unittest' => MODPATH.'unittest'), array('unittest' => $this->dirSeparator(MODPATH.'unittest/'))),
		);
	}

	/**
	 * Tests Kohana::modules()
	 *
	 * @test
	 * @dataProvider provider_modules_sets_and_returns_valid_modules
	 * @param boolean $source   Input for Kohana::modules
	 * @param boolean $expected Output for Kohana::modules
	 */
	public function test_modules_sets_and_returns_valid_modules($source, $expected)
	{
		$modules = Kohana::modules();

		try
		{
			$this->assertEquals($expected, Kohana::modules($source));
		}
		catch(Exception $e)
		{
			Kohana::modules($modules);

			throw $e;
		}

		Kohana::modules($modules);
	}

	/**
	 * To make the tests as portable as possible this just tests that
	 * you get an array of modules when you can Kohana::modules() and that
	 * said array contains unittest
	 *
	 * @test
	 * @covers Kohana::modules
	 */
	public function test_modules_returns_array_of_modules()
	{
		$modules = Kohana::modules();

		$this->assertInternalType('array', $modules);

		$this->assertArrayHasKey('unittest', $modules);
	}

	/**
	 * Tests Kohana::include_paths()
	 *
	 * The include paths must contain the apppath and syspath
	 * @test
	 * @covers Kohana::include_paths
	 */
	public function test_include_paths()
	{
		$include_paths = Kohana::include_paths();
		$modules       = Kohana::modules();

		$this->assertInternalType('array', $include_paths);

		// We must have at least 2 items in include paths (APP / SYS)
		$this->assertGreaterThan(2, count($include_paths));
		// Make sure said paths are in the include paths
		// And make sure they're in the correct positions
		$this->assertSame(APPPATH, reset($include_paths));
		$this->assertSame(SYSPATH, end($include_paths));

		foreach ($modules as $module)
		{
			$this->assertContains($module, $include_paths);
		}
	}
}

