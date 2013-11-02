<?php
include 'helpers/config.php';

class ModelCallbackTest extends DatabaseTest
{
	public function set_up($connection_name=null)
	{
		parent::set_up($connection_name);

		$this->venue = new Venue();
		$this->callback = Venue::table()->callback;
	}

	public function register_and_invoke_callbacks($callbacks, $return, $closure)
	{
		if (!is_array($callbacks))
			$callbacks = array($callbacks);

		$fired = array();

		foreach ($callbacks as $name)
			$this->callback->register($name,function($model) use (&$fired, $name, $return) { $fired[] = $name; return $return; });

		$closure($this->venue);
		return array_intersect($callbacks,$fired);
	}

	public function assert_fires($callbacks, $closure)
	{
		$executed = $this->register_and_invoke_callbacks($callbacks,true,$closure);
		$this->assert_equals(count($callbacks),count($executed));
	}

	public function assert_does_not_fire($callbacks, $closure)
	{
		$executed = $this->register_and_invoke_callbacks($callbacks,true,$closure);
		$this->assert_equals(0,count($executed));
	}

	public function assert_fires_returns_false($callbacks, $only_fire, $closure)
	{
		if (!is_array($only_fire))
			$only_fire = array($only_fire);

		$executed = $this->register_and_invoke_callbacks($callbacks,false,$closure);
		sort($only_fire);
		$intersect = array_intersect($only_fire,$executed);
		sort($intersect);
		$this->assert_equals($only_fire,$intersect);
	}

	public function test_after_construct_fires_by_default()
	{
		$this->assert_fires('after_construct',function($model) { new Venue(); });
	}

	public function test_fire_validation_callbacks_on_insert()
	{
		$this->assert_fires(array('before_validation','after_validation','before_validation_on_create','after_validation_on_create'),
			function($model) { $model = new Venue(); $model->save(); });
	}

	public function test_fire_validation_callbacks_on_update()
	{
		$this->assert_fires(array('before_validation','after_validation','before_validation_on_update','after_validation_on_update'),
			function($model) { $model = Venue::first(); $model->save(); });
	}

	public function test_validation_call_backs_not_fired_due_to_bypassing_validations()
	{
		$this->assert_does_not_fire('before_validation',function($model) { $model->save(false); });
	}

	public function test_before_validation_returning_false_cancels_callbacks()
	{
		$this->assert_fires_returns_false(array('before_validation','after_validation'),'before_validation',
			function($model) { $model->save(); });
	}

	public function test_fires_before_save_and_before_update_when_updating()
	{
		$this->assert_fires(array('before_save','before_update'),
			function($model) { $model = Venue::first(); $model->name = "something new"; $model->save(); });
	}

	public function test_before_save_returning_false_cancels_callbacks()
	{
		$this->assert_fires_returns_false(array('before_save','before_create'),'before_save',
			function($model) { $model = new Venue(); $model->save(); });
	}

	public function test_destroy()
	{
		$this->assert_fires(array('before_destroy','after_destroy'),
			function($model) { $model->delete(); });
	}
}
?>
