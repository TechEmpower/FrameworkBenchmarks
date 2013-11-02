<?php
include 'helpers/config.php';

class BookLength extends ActiveRecord\Model
{
	static $table = 'books';
	static $validates_length_of = array();
}

class BookSize extends ActiveRecord\Model
{
	static $table = 'books';
	static $validates_size_of = array();
}

class ValidatesLengthOfTest extends DatabaseTest
{
	public function set_up($connection_name=null)
	{
		parent::set_up($connection_name);
		BookLength::$validates_length_of[0] = array('name', 'allow_blank' => false, 'allow_null' => false);
	}
	
	public function test_within()
	{
		BookLength::$validates_length_of[0]['within'] = array(1, 5);
		$book = new BookLength;
		$book->name = '12345';
		$book->save();
		$this->assert_false($book->errors->is_invalid('name'));
	}

	public function test_within_error_message()
	{
		BookLength::$validates_length_of[0]['within'] = array(2,5);
		$book = new BookLength();
		$book->name = '1';
		$book->is_valid();
		$this->assert_equals(array('Name is too short (minimum is 2 characters)'),$book->errors->full_messages());

		$book->name = '123456';
		$book->is_valid();
		$this->assert_equals(array('Name is too long (maximum is 5 characters)'),$book->errors->full_messages());
	}

	public function test_within_custom_error_message()
	{
		BookLength::$validates_length_of[0]['within'] = array(2,5);
		BookLength::$validates_length_of[0]['too_short'] = 'is too short';
		BookLength::$validates_length_of[0]['message'] = 'is not between 2 and 5 characters';
		$book = new BookLength();
		$book->name = '1';
		$book->is_valid();
		$this->assert_equals(array('Name is not between 2 and 5 characters'),$book->errors->full_messages());

		$book->name = '123456';
		$book->is_valid();
		$this->assert_equals(array('Name is not between 2 and 5 characters'),$book->errors->full_messages());
	}
	
	public function test_valid_in()
	{
		BookLength::$validates_length_of[0]['in'] = array(1, 5);
		$book = new BookLength;
		$book->name = '12345';
		$book->save();
		$this->assert_false($book->errors->is_invalid('name'));
	}

	public function test_aliased_size_of()
	{
		BookSize::$validates_size_of = BookLength::$validates_length_of;
		BookSize::$validates_size_of[0]['within'] = array(1, 5);
		$book = new BookSize;
		$book->name = '12345';
		$book->save();
		$this->assert_false($book->errors->is_invalid('name'));
	}

	public function test_invalid_within_and_in()
	{
		BookLength::$validates_length_of[0]['within'] = array(1, 3);
		$book = new BookLength;
		$book->name = 'four';
		$book->save();
		$this->assert_true($book->errors->is_invalid('name'));

		$this->set_up();
		BookLength::$validates_length_of[0]['in'] = array(1, 3);
		$book = new BookLength;
		$book->name = 'four';
		$book->save();
		$this->assert_true($book->errors->is_invalid('name'));
	}

	public function test_valid_null()
	{
		BookLength::$validates_length_of[0]['within'] = array(1, 3);
		BookLength::$validates_length_of[0]['allow_null'] = true;

		$book = new BookLength;
		$book->name = null;
		$book->save();
		$this->assert_false($book->errors->is_invalid('name'));
	}

	public function test_valid_blank()
	{
		BookLength::$validates_length_of[0]['within'] = array(1, 3);
		BookLength::$validates_length_of[0]['allow_blank'] = true;

		$book = new BookLength;
		$book->name = '';
		$book->save();
		$this->assert_false($book->errors->is_invalid('name'));
	}

	public function test_invalid_blank()
	{
		BookLength::$validates_length_of[0]['within'] = array(1, 3);

		$book = new BookLength;
		$book->name = '';
		$book->save();
		$this->assert_true($book->errors->is_invalid('name'));
		$this->assert_equals('is too short (minimum is 1 characters)', $book->errors->on('name'));
	}

	public function test_invalid_null_within()
	{
		BookLength::$validates_length_of[0]['within'] = array(1, 3);

		$book = new BookLength;
		$book->name = null;
		$book->save();
		$this->assert_true($book->errors->is_invalid('name'));
		$this->assert_equals('is too short (minimum is 1 characters)', $book->errors->on('name'));
	}
	
	public function test_invalid_null_minimum()
	{
		BookLength::$validates_length_of[0]['minimum'] = 1;

		$book = new BookLength;
		$book->name = null;
		$book->save();
		$this->assert_true($book->errors->is_invalid('name'));
		$this->assert_equals('is too short (minimum is 1 characters)', $book->errors->on('name'));
		
	}
	
	public function test_valid_null_maximum()
	{
		BookLength::$validates_length_of[0]['maximum'] = 1;

		$book = new BookLength;
		$book->name = null;
		$book->save();
		$this->assert_false($book->errors->is_invalid('name'));
	}

	public function test_float_as_impossible_range_option()
	{
		BookLength::$validates_length_of[0]['within'] = array(1, 3.6);
		$book = new BookLength;
		$book->name = '123';
		try {
			$book->save();
		} catch (ActiveRecord\ValidationsArgumentError $e) {
			$this->assert_equals('maximum value cannot use a float for length.', $e->getMessage());
		}

		$this->set_up();
		BookLength::$validates_length_of[0]['is'] = 1.8;
		$book = new BookLength;
		$book->name = '123';
		try {
			$book->save();
		} catch (ActiveRecord\ValidationsArgumentError $e) {
			$this->assert_equals('is value cannot use a float for length.', $e->getMessage());
			return;
		}

		$this->fail('An expected exception has not be raised.');
	}

	public function test_signed_integer_as_impossible_within_option()
	{
		BookLength::$validates_length_of[0]['within'] = array(-1, 3);

		$book = new BookLength;
		$book->name = '123';
		try {
			$book->save();
		} catch (ActiveRecord\ValidationsArgumentError $e) {
			$this->assert_equals('minimum value cannot use a signed integer.', $e->getMessage());
			return;
		}

		$this->fail('An expected exception has not be raised.');
	}

	public function test_signed_integer_as_impossible_is_option()
	{
		BookLength::$validates_length_of[0]['is'] = -8;

		$book = new BookLength;
		$book->name = '123';
		try {
			$book->save();
		} catch (ActiveRecord\ValidationsArgumentError $e) {
			$this->assert_equals('is value cannot use a signed integer.', $e->getMessage());
			return;
		}

		$this->fail('An expected exception has not be raised.');
	}

	public function test_lack_of_option()
	{
		try {
			$book = new BookLength;
			$book->name = null;
			$book->save();
		} catch (ActiveRecord\ValidationsArgumentError $e) {
			$this->assert_equals('Range unspecified.  Specify the [within], [maximum], or [is] option.', $e->getMessage());
			return;
		}

		$this->fail('An expected exception has not be raised.');
	}

	public function test_too_many_options()
	{
		BookLength::$validates_length_of[0]['within'] = array(1, 3);
		BookLength::$validates_length_of[0]['in'] = array(1, 3);

		try {
			$book = new BookLength;
			$book->name = null;
			$book->save();
		} catch (ActiveRecord\ValidationsArgumentError $e) {
			$this->assert_equals('Too many range options specified.  Choose only one.', $e->getMessage());
			return;
		}

		$this->fail('An expected exception has not be raised.');
	}

	public function test_too_many_options_with_different_option_types()
	{
		BookLength::$validates_length_of[0]['within'] = array(1, 3);
		BookLength::$validates_length_of[0]['is'] = 3;

		try {
			$book = new BookLength;
			$book->name = null;
			$book->save();
		} catch (ActiveRecord\ValidationsArgumentError $e) {
			$this->assert_equals('Too many range options specified.  Choose only one.', $e->getMessage());
			return;
		}

		$this->fail('An expected exception has not be raised.');
	}

	/**
	 * @expectedException ActiveRecord\ValidationsArgumentError
	 */
	public function test_with_option_as_non_numeric()
	{
		BookLength::$validates_length_of[0]['with'] = array('test');

		$book = new BookLength;
		$book->name = null;
		$book->save();
	}

	/**
	 * @expectedException ActiveRecord\ValidationsArgumentError
	 */
	public function test_with_option_as_non_numeric_non_array()
	{
		BookLength::$validates_length_of[0]['with'] = 'test';

		$book = new BookLength;
		$book->name = null;
		$book->save();
	}

	public function test_validates_length_of_maximum()
	{
		BookLength::$validates_length_of[0] = array('name', 'maximum' => 10);
		$book = new BookLength(array('name' => '12345678901'));
		$book->is_valid();
		$this->assert_equals(array("Name is too long (maximum is 10 characters)"),$book->errors->full_messages());
	}

	public function test_validates_length_of_minimum()
	{
		BookLength::$validates_length_of[0] = array('name', 'minimum' => 2);
		$book = new BookLength(array('name' => '1'));
		$book->is_valid();
		$this->assert_equals(array("Name is too short (minimum is 2 characters)"),$book->errors->full_messages());
	}
	
	public function test_validates_length_of_min_max_custom_message()
	{
		BookLength::$validates_length_of[0] = array('name', 'maximum' => 10, 'message' => 'is far too long');
		$book = new BookLength(array('name' => '12345678901'));
		$book->is_valid();
		$this->assert_equals(array("Name is far too long"),$book->errors->full_messages());

		BookLength::$validates_length_of[0] = array('name', 'minimum' => 10, 'message' => 'is far too short');
		$book = new BookLength(array('name' => '123456789'));
		$book->is_valid();
		$this->assert_equals(array("Name is far too short"),$book->errors->full_messages());
	}
	
	public function test_validates_length_of_min_max_custom_message_overridden()
	{
		BookLength::$validates_length_of[0] = array('name', 'minimum' => 10, 'too_short' => 'is too short', 'message' => 'is custom message');
		$book = new BookLength(array('name' => '123456789'));
		$book->is_valid();
		$this->assert_equals(array("Name is custom message"),$book->errors->full_messages());
	}

	public function test_validates_length_of_is()
	{
		BookLength::$validates_length_of[0] = array('name', 'is' => 2);
		$book = new BookLength(array('name' => '123'));
		$book->is_valid();
		$this->assert_equals(array("Name is the wrong length (should be 2 characters)"),$book->errors->full_messages());
	}
};
?>