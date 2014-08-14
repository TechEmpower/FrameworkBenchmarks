<?php
class SnakeCase_PHPUnit_Framework_TestCase extends PHPUnit_Framework_TestCase
{
	public function __call($meth, $args)
	{
		$camel_cased_method = ActiveRecord\Inflector::instance()->camelize($meth);

		if (method_exists($this, $camel_cased_method))
			return call_user_func_array(array($this, $camel_cased_method), $args);

		$class_name = get_called_class();
		$trace = debug_backtrace();
		die("PHP Fatal Error:  Call to undefined method $class_name::$meth() in {$trace[1]['file']} on line {$trace[1]['line']}" . PHP_EOL);
	}

	public function setUp()
	{
		if (method_exists($this,'set_up'))
			call_user_func_array(array($this,'set_up'),func_get_args());
	}

	public function tearDown()
	{
		if (method_exists($this,'tear_down'))
			call_user_func_array(array($this,'tear_down'),func_get_args());
	}

	private function setup_assert_keys($args)
	{
		$last = count($args)-1;
		$keys = array_slice($args,0,$last);
		$array = $args[$last];
		return array($keys,$array);
	}

	public function assert_has_keys(/* $keys..., $array */)
	{
		list($keys,$array) = $this->setup_assert_keys(func_get_args());

		$this->assert_not_null($array,'Array was null');

		foreach ($keys as $name)
			$this->assert_array_has_key($name,$array);
	}

	public function assert_doesnt_has_keys(/* $keys..., $array */)
	{
		list($keys,$array) = $this->setup_assert_keys(func_get_args());

		foreach ($keys as $name)
			$this->assert_array_not_has_key($name,$array);
	}

	public function assert_is_a($expected_class, $object)
	{
		$this->assert_equals($expected_class,get_class($object));
	}

	public function assert_datetime_equals($expected, $actual)
	{
		$this->assert_equals($expected->format(DateTime::ISO8601),$actual->format(DateTime::ISO8601));
	}
}
?>