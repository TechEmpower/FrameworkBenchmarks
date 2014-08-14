<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Tests the View class
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.view
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_ViewTest extends Unittest_TestCase
{
	protected static $old_modules = array();

	/**
	 * Setups the filesystem for test view files
	 *
	 * @return null
	 */
	// @codingStandardsIgnoreStart
	public static function setupBeforeClass()
	// @codingStandardsIgnoreEnd
	{
		self::$old_modules = Kohana::modules();

		$new_modules = self::$old_modules+array(
			'test_views' => realpath(dirname(__FILE__).'/../test_data/')
		);
		Kohana::modules($new_modules);
	}

	/**
	 * Restores the module list
	 *
	 * @return null
	 */
	// @codingStandardsIgnoreStart
	public static function teardownAfterClass()
	// @codingStandardsIgnoreEnd
	{
		Kohana::modules(self::$old_modules);
	}

	/**
	 * Provider for test_instaniate
	 *
	 * @return array
	 */
	public function provider_instantiate()
	{
		return array(
			array('kohana/error', FALSE),
			array('test.css', FALSE),
			array('doesnt_exist', TRUE),
		);
	}

	/**
	 * Tests that we can instantiate a view file
	 * 
	 * @test
	 * @dataProvider provider_instantiate
	 *
	 * @return null
	 */
	public function test_instantiate($path, $expects_exception)
	{
		try
		{
			$view = new View($path);
			$this->assertSame(FALSE, $expects_exception);
		}
		catch(View_Exception $e)
		{
			$this->assertSame(TRUE, $expects_exception);
		}
	}
}
