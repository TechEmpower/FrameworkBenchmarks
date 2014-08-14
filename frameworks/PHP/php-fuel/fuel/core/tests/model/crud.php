<?php
/**
 * Part of the Fuel framework
 *
 * @package   Fuel
 * @version   1.0
 * @author    Fuel Development Team
 * @license   MIT License
 * @copyright 2010 - 2012 Fuel Development Team
 * @link      http://fuelphp.com
 */

namespace Fuel\Core;

/**
 * Model_Crud class tests
 *
 * @group Core
 * @group Model
 */
class Test_Model_Crud extends TestCase
{
	public function test_foo() {}

	public function test_get_connection()
	{
		$refl = new \ReflectionClass('\Fuel\Core\Model_Crud_Tester');
		$method = $refl->getMethod('get_connection');
		$method->setAccessible(true);

		$tester = new Model_Crud_Tester();
		$write = $method->invokeArgs($tester, array(true));
		$read = $method->invokeArgs($tester, array(false));

		$this->assertEquals('read', $read);
		$this->assertEquals('write', $write);
	}
}

class Model_Crud_Tester extends \Fuel\Core\Model_Crud
{
	static protected $_connection = "read";

	static protected $_write_connection = "write";
}
