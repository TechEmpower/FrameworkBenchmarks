<?php
include 'helpers/config.php';

class BookFormat extends ActiveRecord\Model
{
	static $table = 'books';
	static $validates_format_of = array(
		array('name')
	);
};

class ValidatesFormatOfTest extends DatabaseTest
{
	public function set_up($connection_name=null)
	{
		parent::set_up($connection_name);
		BookFormat::$validates_format_of[0] = array('name');
	}

	public function test_format()
	{
		BookFormat::$validates_format_of[0]['with'] = '/^[a-z\W]*$/';
		$book = new BookFormat(array('author_id' => 1, 'name' => 'testing reg'));
		$book->save();
		$this->assert_false($book->errors->is_invalid('name'));

		BookFormat::$validates_format_of[0]['with'] = '/[0-9]/';
		$book = new BookFormat(array('author_id' => 1, 'name' => 12));
		$book->save();
		$this->assert_false($book->errors->is_invalid('name'));
	}

	public function test_invalid_null()
	{
		BookFormat::$validates_format_of[0]['with'] = '/[^0-9]/';
		$book = new BookFormat;
		$book->name = null;
		$book->save();
		$this->assert_true($book->errors->is_invalid('name'));
	}

	public function test_invalid_blank()
	{
		BookFormat::$validates_format_of[0]['with'] = '/[^0-9]/';
		$book = new BookFormat;
		$book->name = '';
		$book->save();
		$this->assert_true($book->errors->is_invalid('name'));
	}

	public function test_valid_blank_andallow_blank()
	{
		BookFormat::$validates_format_of[0]['allow_blank'] = true;
		BookFormat::$validates_format_of[0]['with'] = '/[^0-9]/';
		$book = new BookFormat(array('author_id' => 1, 'name' => ''));
		$book->save();
		$this->assert_false($book->errors->is_invalid('name'));
	}

	public function test_valid_null_and_allow_null()
	{
		BookFormat::$validates_format_of[0]['allow_null'] = true;
		BookFormat::$validates_format_of[0]['with'] = '/[^0-9]/';
		$book = new BookFormat();
		$book->author_id = 1;
		$book->name = null;
		$book->save();
		$this->assert_false($book->errors->is_invalid('name'));
	}

	/**
	 * @expectedException ActiveRecord\ValidationsArgumentError
	 */
	public function test_invalid_lack_of_with_key()
	{
		$book = new BookFormat;
		$book->name = null;
		$book->save();
	}

	/**
	 * @expectedException ActiveRecord\ValidationsArgumentError
	 */
	public function test_invalid_with_expression_as_non_string()
	{
		BookFormat::$validates_format_of[0]['with'] = array('test');
		$book = new BookFormat;
		$book->name = null;
		$book->save();
	}

	public function test_invalid_with_expression_as_non_regexp()
	{
		BookFormat::$validates_format_of[0]['with'] = 'blah';
		$book = new BookFormat;
		$book->name = 'blah';
		$book->save();
		$this->assert_true($book->errors->is_invalid('name'));
	}

	public function test_custom_message()
	{
		BookFormat::$validates_format_of[0]['message'] = 'is using a custom message.';
		BookFormat::$validates_format_of[0]['with'] = '/[^0-9]/';

		$book = new BookFormat;
		$book->name = null;
		$book->save();
		$this->assert_equals('is using a custom message.', $book->errors->on('name'));
	}
};
?>