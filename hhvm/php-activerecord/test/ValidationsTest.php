<?php
include 'helpers/config.php';

use ActiveRecord as AR;

class BookValidations extends ActiveRecord\Model
{
	static $table_name = 'books';
	static $alias_attribute = array('name_alias' => 'name', 'x' => 'secondary_author_id');
	static $validates_presence_of = array();
	static $validates_uniqueness_of = array();
	static $custom_validator_error_msg = 'failed custom validation';

	// fired for every validation - but only used for custom validation test
	public function validate()
	{
		if ($this->name == 'test_custom_validation')
			$this->errors->add('name', self::$custom_validator_error_msg);
	}
}

class ValuestoreValidations extends ActiveRecord\Model
{
	static $table_name = 'valuestore';
	static $validates_uniqueness_of = array();
}

class ValidationsTest extends DatabaseTest
{
	public function set_up($connection_name=null)
	{
		parent::set_up($connection_name);

		BookValidations::$validates_presence_of[0] = 'name';
		BookValidations::$validates_uniqueness_of[0] = 'name';
		
		ValuestoreValidations::$validates_uniqueness_of[0] = 'key';
	}

	public function test_is_valid_invokes_validations()
	{
		$book = new Book;
		$this->assert_true(empty($book->errors));
		$book->is_valid();
		$this->assert_false(empty($book->errors));
	}

	public function test_is_valid_returns_true_if_no_validations_exist()
	{
		$book = new Book;
		$this->assert_true($book->is_valid());
	}

	public function test_is_valid_returns_false_if_failed_validations()
	{
		$book = new BookValidations;
		$this->assert_false($book->is_valid());
	}

	public function test_is_invalid()
	{
		$book = new Book();
		$this->assert_false($book->is_invalid());
	}

	public function test_is_invalid_is_true()
	{
		$book = new BookValidations();
		$this->assert_true($book->is_invalid());
	}

	public function test_is_iterable()
	{
		$book = new BookValidations();
		$book->is_valid();

		foreach ($book->errors as $name => $message)
			$this->assert_equals("Name can't be blank",$message);
	}

	public function test_full_messages()
	{
		$book = new BookValidations();
		$book->is_valid();

		$this->assert_equals(array("Name can't be blank"),array_values($book->errors->full_messages(array('hash' => true))));
	}

	public function test_to_array()
	{
		$book = new BookValidations();
		$book->is_valid();

		$this->assert_equals(array("name" => array("Name can't be blank")), $book->errors->to_array());
	}
	
	public function test_toString()
	{
		$book = new BookValidations();
		$book->is_valid();
		$book->errors->add('secondary_author_id', "is invalid");
		
		$this->assert_equals("Name can't be blank\nSecondary author id is invalid", (string) $book->errors);
	}

	public function test_validates_uniqueness_of()
	{
		BookValidations::create(array('name' => 'bob'));
		$book = BookValidations::create(array('name' => 'bob'));

		$this->assert_equals(array("Name must be unique"),$book->errors->full_messages());
		$this->assert_equals(1,BookValidations::count(array('conditions' => "name='bob'")));
	}

	public function test_validates_uniqueness_of_excludes_self()
	{
		$book = BookValidations::first();
		$this->assert_equals(true,$book->is_valid());
	}

	public function test_validates_uniqueness_of_with_multiple_fields()
	{
		BookValidations::$validates_uniqueness_of[0] = array(array('name','special'));
		$book1 = BookValidations::first();
		$book2 = new BookValidations(array('name' => $book1->name, 'special' => $book1->special+1));
		$this->assert_true($book2->is_valid());
	}

	public function test_validates_uniqueness_of_with_multiple_fields_is_not_unique()
	{
		BookValidations::$validates_uniqueness_of[0] = array(array('name','special'));
		$book1 = BookValidations::first();
		$book2 = new BookValidations(array('name' => $book1->name, 'special' => $book1->special));
		$this->assert_false($book2->is_valid());
		$this->assert_equals(array('Name and special must be unique'),$book2->errors->full_messages());
	}

	public function test_validates_uniqueness_of_works_with_alias_attribute()
	{
		BookValidations::$validates_uniqueness_of[0] = array(array('name_alias','x'));
		$book = BookValidations::create(array('name_alias' => 'Another Book', 'x' => 2));
		$this->assert_false($book->is_valid());
		$this->assert_equals(array('Name alias and x must be unique'), $book->errors->full_messages());
	}

	public function test_validates_uniqueness_of_works_with_mysql_reserved_word_as_column_name()
	{
		ValuestoreValidations::create(array('key' => 'GA_KEY', 'value' => 'UA-1234567-1'));
		$valuestore = ValuestoreValidations::create(array('key' => 'GA_KEY', 'value' => 'UA-1234567-2'));

		$this->assert_equals(array("Key must be unique"),$valuestore->errors->full_messages());
		$this->assert_equals(1,ValuestoreValidations::count(array('conditions' => "`key`='GA_KEY'")));
	}

	public function test_get_validation_rules()
	{
		$validators = BookValidations::first()->get_validation_rules();
		$this->assert_true(in_array(array('validator' => 'validates_presence_of'),$validators['name']));
	}

	public function test_model_is_nulled_out_to_prevent_memory_leak()
	{
		$book = new BookValidations();
		$book->is_valid();
		$this->assert_true(strpos(serialize($book->errors),'model";N;') !== false);
	}

	public function test_validations_takes_strings()
	{
		BookValidations::$validates_presence_of = array('numeric_test', array('special'), 'name');
		$book = new BookValidations(array('numeric_test' => 1, 'special' => 1));
		$this->assert_false($book->is_valid());
	}

	public function test_gh131_custom_validation()
	{
		$book = new BookValidations(array('name' => 'test_custom_validation'));
		$book->save();
		$this->assert_true($book->errors->is_invalid('name'));
		$this->assert_equals(BookValidations::$custom_validator_error_msg, $book->errors->on('name'));
	}
};
?>
