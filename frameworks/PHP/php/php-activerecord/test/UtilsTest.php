<?php
include 'helpers/config.php';

use ActiveRecord as AR;

class UtilsTest extends SnakeCase_PHPUnit_Framework_TestCase
{
	public function set_up()
	{
		$this->object_array = array(null,null);
		$this->object_array[0] = new stdClass();
		$this->object_array[0]->a = "0a";
		$this->object_array[0]->b = "0b";
		$this->object_array[1] = new stdClass();
		$this->object_array[1]->a = "1a";
		$this->object_array[1]->b = "1b";

		$this->array_hash = array(
			array("a" => "0a", "b" => "0b"),
			array("a" => "1a", "b" => "1b"));
	}

	public function test_collect_with_array_of_objects_using_closure()
	{
		$this->assert_equals(array("0a","1a"),AR\collect($this->object_array,function($obj) { return $obj->a; }));
	}

	public function test_collect_with_array_of_objects_using_string()
	{
		$this->assert_equals(array("0a","1a"),AR\collect($this->object_array,"a"));
	}

	public function test_collect_with_array_hash_using_closure()
	{
		$this->assert_equals(array("0a","1a"),AR\collect($this->array_hash,function($item) { return $item["a"]; }));
	}

	public function test_collect_with_array_hash_using_string()
	{
		$this->assert_equals(array("0a","1a"),AR\collect($this->array_hash,"a"));
	}

    public function test_array_flatten()
    {
		$this->assert_equals(array(), AR\array_flatten(array()));
		$this->assert_equals(array(1), AR\array_flatten(array(1)));
		$this->assert_equals(array(1), AR\array_flatten(array(array(1))));
		$this->assert_equals(array(1, 2), AR\array_flatten(array(array(1, 2))));
		$this->assert_equals(array(1, 2), AR\array_flatten(array(array(1), 2)));
		$this->assert_equals(array(1, 2), AR\array_flatten(array(1, array(2))));
		$this->assert_equals(array(1, 2, 3), AR\array_flatten(array(1, array(2), 3)));
		$this->assert_equals(array(1, 2, 3, 4), AR\array_flatten(array(1, array(2, 3), 4)));
		$this->assert_equals(array(1, 2, 3, 4, 5, 6), AR\array_flatten(array(1, array(2, 3), 4, array(5, 6))));
	}

	public function test_all()
	{
		$this->assert_true(AR\all(null,array(null,null)));
		$this->assert_true(AR\all(1,array(1,1)));
		$this->assert_false(AR\all(1,array(1,'1')));
		$this->assert_false(AR\all(null,array('',null)));
	}

	public function test_classify()
	{
		$bad_class_names = array('ubuntu_rox', 'stop_the_Snake_Case', 'CamelCased', 'camelCased');
		$good_class_names = array('UbuntuRox', 'StopTheSnakeCase', 'CamelCased', 'CamelCased');

		$class_names = array();
		foreach ($bad_class_names as $s)
			$class_names[] = AR\classify($s);

		$this->assert_equals($class_names, $good_class_names);
	}

	public function test_classify_singularize()
	{
		$bad_class_names = array('events', 'stop_the_Snake_Cases', 'angry_boxes', 'Mad_Sheep_herders', 'happy_People');
		$good_class_names = array('Event', 'StopTheSnakeCase', 'AngryBox', 'MadSheepHerder', 'HappyPerson');

		$class_names = array();
		foreach ($bad_class_names as $s)
			$class_names[] = AR\classify($s, true);

		$this->assert_equals($class_names, $good_class_names);
	}

	public function test_singularize()
	{
		$this->assert_equals('order_status',AR\Utils::singularize('order_status'));
		$this->assert_equals('order_status',AR\Utils::singularize('order_statuses'));
		$this->assert_equals('os_type', AR\Utils::singularize('os_type'));
		$this->assert_equals('os_type', AR\Utils::singularize('os_types'));
		$this->assert_equals('photo', AR\Utils::singularize('photos'));
		$this->assert_equals('pass', AR\Utils::singularize('pass'));
		$this->assert_equals('pass', AR\Utils::singularize('passes'));
	}

	public function test_wrap_strings_in_arrays()
	{
		$x = array('1',array('2'));
		$this->assert_equals(array(array('1'),array('2')),ActiveRecord\wrap_strings_in_arrays($x));

		$x = '1';
		$this->assert_equals(array(array('1')),ActiveRecord\wrap_strings_in_arrays($x));
	}
};
?>
