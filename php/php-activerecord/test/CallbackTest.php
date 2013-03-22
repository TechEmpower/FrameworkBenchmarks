<?php
include 'helpers/config.php';

class CallBackTest extends DatabaseTest
{
	public function set_up($connection_name=null)
	{
		parent::set_up($connection_name);

		// ensure VenueCB model has been loaded
		VenueCB::find(1);

		$this->callback = new ActiveRecord\CallBack('VenueCB');
	}

	public function assert_has_callback($callback_name, $method_name=null)
	{
		if (!$method_name)
			$method_name = $callback_name;

		$this->assert_true(in_array($method_name,$this->callback->get_callbacks($callback_name)));
	}

	public function assert_implicit_save($first_method, $second_method)
	{
		$i_ran = array();
		$this->callback->register($first_method,function($model) use (&$i_ran, $first_method) { $i_ran[] = $first_method; });
		$this->callback->register($second_method,function($model) use (&$i_ran, $second_method) { $i_ran[] = $second_method; });
		$this->callback->invoke(null,$second_method);
		$this->assert_equals(array($first_method,$second_method),$i_ran);
	}

	public function test_generic_callback_was_auto_registered()
	{
		$this->assert_has_callback('after_construct');
	}

	public function test_register()
	{
		$this->callback->register('after_construct');
		$this->assert_has_callback('after_construct');
	}

	public function test_register_non_generic()
	{
		$this->callback->register('after_construct','non_generic_after_construct');
		$this->assert_has_callback('after_construct','non_generic_after_construct');
	}

	/**
	 * @expectedException ActiveRecord\ActiveRecordException
	 */
	public function test_register_invalid_callback()
	{
		$this->callback->register('invalid_callback');
	}

	/**
	 * @expectedException ActiveRecord\ActiveRecordException
	 */
	public function test_register_callback_with_undefined_method()
	{
		$this->callback->register('after_construct','do_not_define_me');
	}

	public function test_register_with_string_definition()
	{
		$this->callback->register('after_construct','after_construct');
		$this->assert_has_callback('after_construct');
	}

	public function test_register_with_closure()
	{
		$this->callback->register('after_construct',function($mode) { });
	}

	public function test_register_with_null_definition()
	{
		$this->callback->register('after_construct',null);
		$this->assert_has_callback('after_construct');
	}

	public function test_register_with_no_definition()
	{
		$this->callback->register('after_construct');
		$this->assert_has_callback('after_construct');
	}

	public function test_register_appends_to_registry()
	{
		$this->callback->register('after_construct');
		$this->callback->register('after_construct','non_generic_after_construct');
		$this->assert_equals(array('after_construct','after_construct','non_generic_after_construct'),$this->callback->get_callbacks('after_construct'));
	}

	public function test_register_prepends_to_registry()
	{
		$this->callback->register('after_construct');
		$this->callback->register('after_construct','non_generic_after_construct',array('prepend' => true));
		$this->assert_equals(array('non_generic_after_construct','after_construct','after_construct'),$this->callback->get_callbacks('after_construct'));
	}

	public function test_registers_via_static_array_definition()
	{
		$this->assert_has_callback('after_destroy','after_destroy_one');
		$this->assert_has_callback('after_destroy','after_destroy_two');
	}

	public function test_registers_via_static_string_definition()
	{
		$this->assert_has_callback('before_destroy','before_destroy_using_string');
	}

	/**
	 * @expectedException ActiveRecord\ActiveRecordException
	 */
	public function test_register_via_static_with_invalid_definition()
	{
		$class_name = "Venues_" . md5(uniqid());
		eval("class $class_name extends ActiveRecord\\Model { static \$table_name = 'venues'; static \$after_save = 'method_that_does_not_exist'; };");
		new $class_name();
		new ActiveRecord\CallBack($class_name);
	}

	public function test_can_register_same_multiple_times()
	{
		$this->callback->register('after_construct');
		$this->callback->register('after_construct');
		$this->assert_equals(array('after_construct','after_construct','after_construct'),$this->callback->get_callbacks('after_construct'));
	}

	public function test_register_closure_callback()
	{
		$closure = function($model) {};
		$this->callback->register('after_save',$closure);
		$this->assert_equals(array($closure),$this->callback->get_callbacks('after_save'));
	}

	public function test_get_callbacks_returns_array()
	{
		$this->callback->register('after_construct');
		$this->assert_true(is_array($this->callback->get_callbacks('after_construct')));
	}

	public function test_get_callbacks_returns_null()
	{
		$this->assert_null($this->callback->get_callbacks('this_callback_name_should_never_exist'));
	}

	public function test_invoke_runs_all_callbacks()
	{
		$mock = $this->get_mock('VenueCB',array('after_destroy_one','after_destroy_two'));
		$mock->expects($this->once())->method('after_destroy_one');
		$mock->expects($this->once())->method('after_destroy_two');
		$this->callback->invoke($mock,'after_destroy');
	}

	public function test_invoke_closure()
	{
		$i_ran = false;
		$this->callback->register('after_validation',function($model) use (&$i_ran) { $i_ran = true; });
		$this->callback->invoke(null,'after_validation');
		$this->assert_true($i_ran);
	}

	public function test_invoke_implicitly_calls_save_first()
	{
		$this->assert_implicit_save('before_save','before_create');
		$this->assert_implicit_save('before_save','before_update');
		$this->assert_implicit_save('after_save','after_create');
		$this->assert_implicit_save('after_save','after_update');
	}

	/**
	 * @expectedException ActiveRecord\ActiveRecordException
	 */
	public function test_invoke_unregistered_callback()
	{
		$mock = $this->get_mock('VenueCB', array('columns'));
		$this->callback->invoke($mock,'before_validation_on_create');
	}

	public function test_before_callbacks_pass_on_false_return_callback_returned_false()
	{
		$this->callback->register('before_validation',function($model) { return false; });
		$this->assert_false($this->callback->invoke(null,'before_validation'));
	}

	public function test_before_callbacks_does_not_pass_on_false_for_after_callbacks()
	{
		$this->callback->register('after_validation',function($model) { return false; });
		$this->assert_true($this->callback->invoke(null,'after_validation'));
	}

	public function test_gh_28_after_create_should_be_invoked_after_auto_incrementing_pk_is_set()
	{
		$that = $this;
		VenueCB::$after_create = function($model) use ($that) { $that->assert_not_null($model->id); };
		ActiveRecord\Table::clear_cache('VenueCB');
		$venue = VenueCB::find(1);
		$venue = new VenueCB($venue->attributes());
		$venue->id = null;
		$venue->name = 'alksdjfs';
		$venue->save();
	}

	public function test_before_create_returned_false_halts_execution()
	{
		VenueCB::$before_create = array('before_create_halt_execution');
		ActiveRecord\Table::clear_cache('VenueCB');
		$table = ActiveRecord\Table::load('VenueCB');

		$i_ran = false;
		$i_should_have_ran = false;
		$table->callback->register('before_save', function($model) use (&$i_should_have_ran) { $i_should_have_ran = true; });
		$table->callback->register('before_create',function($model) use (&$i_ran) { $i_ran = true; });
		$table->callback->register('after_create',function($model) use (&$i_ran) { $i_ran = true; });

		$v = VenueCB::find(1);
		$v->id = null;
		VenueCB::create($v->attributes());

		$this->assert_true($i_should_have_ran);
		$this->assert_false($i_ran);
		$this->assert_true(strpos(ActiveRecord\Table::load('VenueCB')->last_sql, 'INSERT') === false);
	}

	public function test_before_save_returned_false_halts_execution()
	{
		VenueCB::$before_update = array('before_update_halt_execution');
		ActiveRecord\Table::clear_cache('VenueCB');
		$table = ActiveRecord\Table::load('VenueCB');

		$i_ran = false;
		$i_should_have_ran = false;
		$table->callback->register('before_save', function($model) use (&$i_should_have_ran) { $i_should_have_ran = true; });
		$table->callback->register('before_update',function($model) use (&$i_ran) { $i_ran = true; });
		$table->callback->register('after_save',function($model) use (&$i_ran) { $i_ran = true; });

		$v = VenueCB::find(1);
		$v->name .= 'test';
		$ret = $v->save();

		$this->assert_true($i_should_have_ran);
		$this->assert_false($i_ran);
		$this->assert_false($ret);
		$this->assert_true(strpos(ActiveRecord\Table::load('VenueCB')->last_sql, 'UPDATE') === false);
	}

	public function test_before_destroy_returned_false_halts_execution()
	{
		VenueCB::$before_destroy = array('before_destroy_halt_execution');
		ActiveRecord\Table::clear_cache('VenueCB');
		$table = ActiveRecord\Table::load('VenueCB');

		$i_ran = false;
		$table->callback->register('before_destroy',function($model) use (&$i_ran) { $i_ran = true; });
		$table->callback->register('after_destroy',function($model) use (&$i_ran) { $i_ran = true; });

		$v = VenueCB::find(1);
		$ret = $v->delete();

		$this->assert_false($i_ran);
		$this->assert_false($ret);
		$this->assert_true(strpos(ActiveRecord\Table::load('VenueCB')->last_sql, 'DELETE') === false);
	}

	public function test_before_validation_returned_false_halts_execution()
	{
		VenueCB::$before_validation = array('before_validation_halt_execution');
		ActiveRecord\Table::clear_cache('VenueCB');
		$table = ActiveRecord\Table::load('VenueCB');

		$v = VenueCB::find(1);
		$v->name .= 'test';
		$ret = $v->save();

		$this->assert_false($ret);
		$this->assert_true(strpos(ActiveRecord\Table::load('VenueCB')->last_sql, 'UPDATE') === false);
	}
};
?>