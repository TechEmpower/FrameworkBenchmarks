<?php
include 'helpers/config.php';

use ActiveRecord\Config;
use ActiveRecord\ConfigException;

class TestLogger
{
	private function log() {}
}

class ConfigTest extends SnakeCase_PHPUnit_Framework_TestCase
{
	public function set_up()
	{
		$this->config = new Config();
		$this->connections = array('development' => 'mysql://blah/development', 'test' => 'mysql://blah/test');
		$this->config->set_connections($this->connections);
	}

	/**
	 * @expectedException ActiveRecord\ConfigException
	 */
	public function test_set_connections_must_be_array()
	{
		$this->config->set_connections(null);
	}

	public function test_get_connections()
	{
		$this->assert_equals($this->connections,$this->config->get_connections());
	}

	public function test_get_connection()
	{
		$this->assert_equals($this->connections['development'],$this->config->get_connection('development'));
	}

	public function test_get_invalid_connection()
	{
		$this->assert_null($this->config->get_connection('whiskey tango foxtrot'));
	}

	public function test_get_default_connection_and_connection()
	{
		$this->config->set_default_connection('development');
		$this->assert_equals('development',$this->config->get_default_connection());
		$this->assert_equals($this->connections['development'],$this->config->get_default_connection_string());
	}

	public function test_get_default_connection_and_connection_string_defaults_to_development()
	{
		$this->assert_equals('development',$this->config->get_default_connection());
		$this->assert_equals($this->connections['development'],$this->config->get_default_connection_string());
	}

	public function test_get_default_connection_string_when_connection_name_is_not_valid()
	{
		$this->config->set_default_connection('little mac');
		$this->assert_null($this->config->get_default_connection_string());
	}

	public function test_default_connection_is_set_when_only_one_connection_is_present()
	{
		$this->config->set_connections(array('development' => $this->connections['development']));
		$this->assert_equals('development',$this->config->get_default_connection());
	}

	public function test_set_connections_with_default()
	{
		$this->config->set_connections($this->connections,'test');
		$this->assert_equals('test',$this->config->get_default_connection());
	}

	public function test_initialize_closure()
	{
		$test = $this;

		Config::initialize(function($cfg) use ($test)
		{
			$test->assert_not_null($cfg);
			$test->assert_equals('ActiveRecord\Config',get_class($cfg));
		});
	}

	public function test_logger_object_must_implement_log_method()
	{
		try {
			$this->config->set_logger(new TestLogger);
			$this->fail();
		} catch (ConfigException $e) {
			$this->assert_equals($e->getMessage(), "Logger object must implement a public log method");
		}
	}
}
?>
