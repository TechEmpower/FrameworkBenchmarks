<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * This test only really exists for code coverage.
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.model
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @author     BRMatt <matthew@sigswitch.com>
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_ModelTest extends Unittest_TestCase
{
	/**
	 * Test the model's factory.
	 *
	 * @test
	 * @covers Model::factory
	 */
	public function test_create()
	{
		$foobar = Model::factory('Foobar');

		$this->assertEquals(TRUE, $foobar instanceof Model);
	}
}

class Model_Foobar extends Model
{

}
