<?php
include 'helpers/config.php';

class BookNumericality extends ActiveRecord\Model
{
	static $table_name = 'books';

	static $validates_numericality_of = array(
		array('name')
	);
}

class ValidatesNumericalityOfTest extends DatabaseTest
{
	static $NULL = array(null);
	static $BLANK = array("", " ", " \t \r \n");
	static $FLOAT_STRINGS = array('0.0','+0.0','-0.0','10.0','10.5','-10.5','-0.0001','-090.1');
	static $INTEGER_STRINGS = array('0', '+0', '-0', '10', '+10', '-10', '0090', '-090');
	static $FLOATS = array(0.0, 10.0, 10.5, -10.5, -0.0001);
	static $INTEGERS = array(0, 10, -10);
	static $JUNK = array("not a number", "42 not a number", "00-1", "--3", "+-3", "+3-1", "-+019.0", "12.12.13.12", "123\nnot a number");

	public function set_up($connection_name=null)
	{
		parent::set_up($connection_name);
		BookNumericality::$validates_numericality_of = array(
			array('numeric_test')
		);
	}

	private function assert_validity($value, $boolean, $msg=null)
	{
		$book = new BookNumericality;
		$book->numeric_test = $value;

		if ($boolean == 'valid')
		{
			$this->assert_true($book->save());
			$this->assert_false($book->errors->is_invalid('numeric_test'));
		}
		else
		{
			$this->assert_false($book->save());
			$this->assert_true($book->errors->is_invalid('numeric_test'));

			if (!is_null($msg))
				$this->assert_same($msg, $book->errors->on('numeric_test'));
		}
	}

	private function assert_invalid($values, $msg=null)
	{
		foreach ($values as $value)
			$this->assert_validity($value, 'invalid', $msg);
	}

	private function assert_valid($values, $msg=null)
	{
		foreach ($values as $value)
			$this->assert_validity($value, 'valid', $msg);
	}

	public function test_numericality()
	{
		//$this->assert_invalid(array("0xdeadbeef"));

		$this->assert_valid(array_merge(self::$FLOATS, self::$INTEGERS));
		$this->assert_invalid(array_merge(self::$NULL, self::$BLANK, self::$JUNK));
	}

	public function test_not_anumber()
	{
		$this->assert_invalid(array('blah'), 'is not a number');
	}

	public function test_invalid_null()
	{
		$this->assert_invalid(array(null));
	}

	public function test_invalid_blank()
	{
		$this->assert_invalid(array(' ', '  '), 'is not a number');
	}

	public function test_invalid_whitespace()
	{
		$this->assert_invalid(array(''));
	}

	public function test_valid_null()
	{
		BookNumericality::$validates_numericality_of[0]['allow_null'] = true;
		$this->assert_valid(array(null));
	}

	public function test_only_integer()
	{
		BookNumericality::$validates_numericality_of[0]['only_integer'] = true;

		$this->assert_valid(array(1, '1'));
		$this->assert_invalid(array(1.5, '1.5'));
	}

	public function test_only_integer_matching_does_not_ignore_other_options()
	{
		BookNumericality::$validates_numericality_of[0]['only_integer'] = true;
		BookNumericality::$validates_numericality_of[0]['greater_than'] = 0;

		$this->assert_invalid(array(-1,'-1'));
	}

	public function test_greater_than()
	{
		BookNumericality::$validates_numericality_of[0]['greater_than'] = 5;

		$this->assert_valid(array(6, '7'));
		$this->assert_invalid(array(5, '5'), 'must be greater than 5');
	}

	public function test_greater_than_or_equal_to()
	{
		BookNumericality::$validates_numericality_of[0]['greater_than_or_equal_to'] = 5;

		$this->assert_valid(array(5, 5.1, '5.1'));
		$this->assert_invalid(array(-50, 4.9, '4.9','-5.1'));
	}

	public function test_less_than()
	{
		BookNumericality::$validates_numericality_of[0]['less_than'] = 5;

		$this->assert_valid(array(4.9, -1, 0, '-5'));
		$this->assert_invalid(array(5, '5'), 'must be less than 5');
	}

	public function test_less_than_or_equal_to()
	{
		BookNumericality::$validates_numericality_of[0]['less_than_or_equal_to'] = 5;

		$this->assert_valid(array(5, -1, 0, 4.9, '-5'));
		$this->assert_invalid(array('8', 5.1), 'must be less than or equal to 5');
	}

	public function test_greater_than_less_than_and_even()
	{
		BookNumericality::$validates_numericality_of[0] = array('numeric_test', 'greater_than' => 1, 'less_than' => 4, 'even' => true);

		$this->assert_valid(array(2));
		$this->assert_invalid(array(1,3,4));
	}

	public function test_custom_message()
	{
		BookNumericality::$validates_numericality_of = array(
			array('numeric_test', 'message' => 'Hello')
		);
		$book = new BookNumericality(array('numeric_test' => 'NaN'));
		$book->is_valid();
		$this->assert_equals(array('Numeric test Hello'),$book->errors->full_messages());
	}
};

array_merge(ValidatesNumericalityOfTest::$INTEGERS, ValidatesNumericalityOfTest::$INTEGER_STRINGS);
array_merge(ValidatesNumericalityOfTest::$FLOATS, ValidatesNumericalityOfTest::$FLOAT_STRINGS);
?>
