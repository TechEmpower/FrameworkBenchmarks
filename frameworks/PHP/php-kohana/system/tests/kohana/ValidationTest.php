<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Tests the Validation lib that's shipped with Kohana
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.validation
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @author     BRMatt <matthew@sigswitch.com>
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_ValidationTest extends Unittest_TestCase
{
	/**
	 * Tests Validation::factory()
	 *
	 * Makes sure that the factory method returns an instance of Validation lib
	 * and that it uses the variables passed
	 *
	 * @test
	 */
	public function test_factory_method_returns_instance_with_values()
	{
		$values = array(
			'this'			=> 'something else',
			'writing tests' => 'sucks',
			'why the hell'	=> 'amIDoingThis',
		);

		$instance = Validation::factory($values);

		$this->assertTrue($instance instanceof Validation);

		$this->assertSame(
			$values,
			$instance->data()
		);
	}

	/**
	 * When we copy() a validation object, we should have a new validation object
	 * with the exact same attributes, apart from the data, which should be the
	 * same as the array we pass to copy()
	 *
	 * @test
	 * @covers Validation::copy
	 */
	public function test_copy_copies_all_attributes_except_data()
	{
		$validation = new Validation(array('foo' => 'bar', 'fud' => 'fear, uncertainty, doubt', 'num' => 9));

		$validation->rule('num', 'is_int')->rule('foo', 'is_string');

		$copy_data = array('foo' => 'no', 'fud' => 'maybe', 'num' => 42);

		$copy = $validation->copy($copy_data);

		$this->assertNotSame($validation, $copy);

		foreach (array('_rules', '_bound', '_labels', '_empty_rules', '_errors') as $attribute)
		{
			// This is just an easy way to check that the attributes are identical
			// Without hardcoding the expected values
			$this->assertAttributeSame(
				self::readAttribute($validation, $attribute),
				$attribute,
				$copy
			);
		}

		$this->assertSame($copy_data, $copy->data());
	}

	/**
	 * When the validation object is initially created there should be no labels
	 * specified
	 *
	 * @test
	 */
	public function test_initially_there_are_no_labels()
	{
		$validation = new Validation(array());

		$this->assertAttributeSame(array(), '_labels', $validation);
	}

	/**
	 * Adding a label to a field should set it in the labels array
	 * If the label already exists it should overwrite it
	 *
	 * In both cases thefunction should return a reference to $this
	 *
	 * @test
	 * @covers Validation::label
	 */
	public function test_label_adds_and_overwrites_label_and_returns_this()
	{
		$validation = new Validation(array());

		$this->assertSame($validation, $validation->label('email', 'Email Address'));

		$this->assertAttributeSame(array('email' => 'Email Address'), '_labels', $validation);

		$this->assertSame($validation, $validation->label('email', 'Your Email'));

		$validation->label('name', 'Your Name');

		$this->assertAttributeSame(
			array('email' => 'Your Email', 'name' => 'Your Name'),
			'_labels',
			$validation
		);
	}

	/**
	 * Using labels() we should be able to add / overwrite multiple labels
	 *
	 * The function should also return $this for chaining purposes
	 *
	 * @test
	 * @covers Validation::labels
	 */
	public function test_labels_adds_and_overwrites_multiple_labels_and_returns_this()
	{
		$validation = new Validation(array());
		$initial_data = array('kung fu' => 'fighting', 'fast' => 'cheetah');

		$this->assertSame($validation, $validation->labels($initial_data));

		$this->assertAttributeSame($initial_data, '_labels', $validation);

		$this->assertSame($validation, $validation->labels(array('fast' => 'lightning')));

		$this->assertAttributeSame(
			array('fast' => 'lightning', 'kung fu' => 'fighting'),
			'_labels',
			$validation
		);
	}

	/**
	 * Using bind() we should be able to add / overwrite multiple bound variables
	 *
	 * The function should also return $this for chaining purposes
	 *
	 * @test
	 * @covers Validation::bind
	 */
	public function test_bind_adds_and_overwrites_multiple_variables_and_returns_this()
	{
		$validation = new Validation(array());
		$data = array('kung fu' => 'fighting', 'fast' => 'cheetah');
		$bound = array(':foo' => 'some value');

		// Test binding an array of values
		$this->assertSame($validation, $validation->bind($bound));
		$this->assertAttributeSame($bound, '_bound', $validation);

		// Test binding one value
		$this->assertSame($validation, $validation->bind(':foo', 'some other value'));
		$this->assertAttributeSame(array(':foo' => 'some other value'), '_bound', $validation);
	}

	/**
	 * We should be able to used bound variables in callbacks
	 *
	 * @test
	 * @covers Validation::check
	 */
	public function test_bound_callback()
	{
		$data = array(
			'kung fu' => 'fighting',
			'fast'    => 'cheetah',
		);
		$validation = new Validation($data);
		$validation->bind(':class', 'Valid')
			// Use the bound value in a callback
			->rule('fast', array(':class', 'max_length'), array(':value', 2));

		// The rule should have run and check() should fail
		$this->assertSame($validation->check(), FALSE);
	}

	/**
	 * Provides test data for test_check
	 *
	 * @return array
	 */
	public function provider_check()
	{
		// $data_array, $rules, $labels, $first_expected, $expected_error
		return array(
			array(
				array('foo' => 'bar'),
				array('foo' => array(array('not_empty', NULL))),
				array(),
				TRUE,
				array(),
			),
			array(
				array('unit' => 'test'),
				array(
					'foo'  => array(array('not_empty', NULL)),
					'unit' => array(array('min_length', array(':value', 6))
					),
				),
				array(),
				FALSE,
				array(
					'foo' => 'foo must not be empty',
					'unit' => 'unit must be at least 6 characters long'
				),
			),
			array(
				array('foo' => 'bar'),
				array(
					// Tests wildcard rules
					TRUE => array(array('min_length', array(':value', 4))),
					'foo'  => array(
						array('not_empty', NULL),
						// Tests the array syntax for callbacks
						array(array('Valid', 'exact_length'), array(':value', 3)),
						// Tests the Class::method syntax for callbacks
						array('Valid::exact_length', array(':value', 3)),
						// Tests the lambda function syntax for callbacks
						// Commented out for PHP 5.2 support
						// array(function($value){return TRUE;}, array(':value')),
						// Tests using a function as a rule
						array('is_string', array(':value')),
					),
					// Tests that rules do not run on empty fields unless they are in _empty_rules
					'unit' => array(array('exact_length', array(':value', 4))),
				),
				array(),
				FALSE,
				array('foo' => 'foo must be at least 4 characters long'),
			),
			// Switch things around and make :value an array
			array(
				array('foo' => array('test', 'data')),
				array('foo' => array(array('in_array', array('kohana', ':value')))),
				array(),
				FALSE,
				array('foo' => 'foo must be one of the available options'),
			),
			// Test wildcard rules with no other rules
			array(
				array('foo' => array('test')),
				array(TRUE => array(array('is_string', array(':value')))),
				array('foo' => 'foo'),
				FALSE,
				array('foo' => '1.foo.is_string'),
			),
			// Test array rules use method as error name
			array(
				array('foo' => 'test'),
				array('foo' => array(array(array('Valid', 'min_length'), array(':value', 10)))),
				array(),
				FALSE,
				array('foo' => 'foo must be at least 10 characters long'),
			),
		);
	}

	/**
	 * Tests Validation::check()
	 *
	 * @test
	 * @covers Validation::check
	 * @covers Validation::rule
	 * @covers Validation::rules
	 * @covers Validation::errors
	 * @covers Validation::error
	 * @dataProvider provider_check
	 * @param array   $array            The array of data
	 * @param array   $rules            The array of rules
	 * @param array   $labels           The array of labels
	 * @param boolean $expected         Is it valid?
	 * @param boolean $expected_errors  Array of expected errors
	 */
	public function test_check($array, $rules, $labels, $expected, $expected_errors)
	{
		$validation = new Validation($array);

		foreach ($labels as $field => $label)
		{
			$validation->label($field, $label);
		}

		foreach ($rules as $field => $field_rules)
		{
			foreach ($field_rules as $rule)
				$validation->rule($field, $rule[0], $rule[1]);
		}

		$status = $validation->check();
		$errors = $validation->errors(TRUE);
		$this->assertSame($expected, $status);
		$this->assertSame($expected_errors, $errors);

		$validation = new validation($array);
		foreach ($rules as $field => $rules)
		{
			$validation->rules($field, $rules);
		}
		$validation->labels($labels);

		$this->assertSame($expected, $validation->check());
	}

	/**
	 * Tests Validation::check()
	 *
	 * @test
	 * @covers Validation::check
	 */
	public function test_check_stops_when_error_added_by_callback()
	{
		$validation = new Validation(array(
			'foo' => 'foo',
		));

		$validation
			->rule('foo', array($this, '_validation_callback'), array(':validation'))
			// This rule should never run
			->rule('foo', 'min_length', array(':value', 20));

		$validation->check();
		$errors = $validation->errors();

		$expected = array(
			'foo' => array(
				0 => '_validation_callback',
				1 => NULL,
			),
		);

		$this->assertSame($errors, $expected);
	}

	public function _validation_callback(Validation $object)
	{
		// Simply add the error
		$object->error('foo', '_validation_callback');
	}

	/**
	 * Provides test data for test_errors()
	 *
	 * @return array
	 */
	public function provider_errors()
	{
		// [data, rules, expected], ...
		return array(
			// No Error
			array(
				array('username' => 'frank'),
				array('username' => array(array('not_empty', NULL))),
				array(),
			),
			// Error from message file
			array(
				array('username' => ''),
				array('username' => array(array('not_empty', NULL))),
				array('username' => 'username must not be empty'),
			),
			// No error message exists, display the path expected
			array(
				array('username' => 'John'),
				array('username' => array(array('strpos', array(':value', 'Kohana')))),
				array('username' => 'Validation.username.strpos'),
			),
		);
	}

	/**
	 * Tests Validation::errors()
	 *
	 * @test
	 * @covers Validation::errors
	 * @dataProvider provider_errors
	 * @param array $array     The array of data
	 * @param array $rules     The array of rules
	 * @param array $expected  Array of expected errors
	 */
	public function test_errors($array, $rules, $expected)
	{
		$validation = Validation::factory($array);

		foreach ($rules as $field => $field_rules)
		{
			$validation->rules($field, $field_rules);
		}

		$validation->check();

		$this->assertSame($expected, $validation->errors('Validation', FALSE));
		// Should be able to get raw errors array
		$this->assertAttributeSame($validation->errors(NULL), '_errors', $validation);
	}

	/**
	 * Provides test data for test_translated_errors()
	 *
	 * @return array
	 */
	public function provider_translated_errors()
	{
		// [data, rules, expected], ...
		return array(
			array(
				array('Spanish' => ''),
				array('Spanish' => array(array('not_empty', NULL))),
				// Errors are not translated yet so only the label will translate
				array('Spanish' => 'Español must not be empty'),
				array('Spanish' => 'Spanish must not be empty'),
			),
		);
	}

	/**
	 * Tests Validation::errors()
	 *
	 * @test
	 * @covers Validation::errors
	 * @dataProvider provider_translated_errors
	 * @param array   $data                   The array of data to test
	 * @param array   $rules                  The array of rules to add
	 * @param array   $translated_expected    The array of expected errors when translated
	 * @param array   $untranslated_expected  The array of expected errors when not translated
	 */
	public function test_translated_errors($data, $rules, $translated_expected, $untranslated_expected)
	{
		$validation = Validation::factory($data);

		$current = i18n::lang();
		i18n::lang('es');

		foreach ($rules as $field => $field_rules)
		{
			$validation->rules($field, $field_rules);
		}

		$validation->check();

		$result_1 = $validation->errors('Validation', TRUE);
		$result_2 = $validation->errors('Validation', 'en');
		$result_3 = $validation->errors('Validation', FALSE);

		// Restore the current language
		i18n::lang($current);

		$this->assertSame($translated_expected, $result_1);
		$this->assertSame($translated_expected, $result_2);
		$this->assertSame($untranslated_expected, $result_3);
	}

	/**
	 * Tests Validation::errors()
	 *
	 * @test
	 * @covers Validation::errors
	 */
	public function test_parameter_labels()
	{
		$validation = Validation::factory(array('foo' => 'bar'))
			->rule('foo', 'equals', array(':value', 'something'))
			->label('something', 'Spanish');

		$current = i18n::lang();
		i18n::lang('es');

		$validation->check();

		$translated_expected = array('foo' => 'foo must equal Español');
		$untranslated_expected = array('foo' => 'foo must equal Spanish');

		$result_1 = $validation->errors('Validation', TRUE);
		$result_2 = $validation->errors('Validation', 'en');
		$result_3 = $validation->errors('Validation', FALSE);

		// Restore the current language
		i18n::lang($current);

		$this->assertSame($translated_expected, $result_1);
		$this->assertSame($translated_expected, $result_2);
		$this->assertSame($untranslated_expected, $result_3);
	}

	/**
	 * Tests Validation::errors()
	 *
	 * @test
	 * @covers Validation::errors
	 */
	public function test_arrays_in_parameters()
	{
		$validation = Validation::factory(array('foo' => 'bar'))
			->rule('foo', 'equals', array(':value', array('one', 'two')));

		$validation->check();

		$expected = array('foo' => 'foo must equal one, two');

		$this->assertSame($expected, $validation->errors('Validation', FALSE));
	}

	/**
	 * Tests Validation::check()
	 *
	 * @test
	 * @covers Validation::check
	 */
	public function test_data_stays_unaltered()
	{
		$validation = Validation::factory(array('foo' => 'bar'))
			->rule('something', 'not_empty');

		$before = $validation->data();
		$validation->check();
		$after = $validation->data();

		$expected = array('foo' => 'bar');

		$this->assertSame($expected, $before);
		$this->assertSame($expected, $after);
	}

	/**
	 * Tests Validation::errors()
	 *
	 * @test
	 * @covers Validation::errors
	 */
	public function test_object_parameters_not_in_messages()
	{
		$validation = Validation::factory(array('foo' => 'foo'))
			->rule('bar', 'matches', array(':validation', ':field', 'foo'));

		$validation->check();
		$errors = $validation->errors('validation');
		$expected = array('bar' => 'bar must be the same as foo');

		$this->assertSame($expected, $errors);
	}

	/**
	 * Tests Validation::as_array()
	 *
	 * @test
	 * @covers Validation::as_array
	 */
	public function test_as_array_returns_original_array()
	{
		$data = array(
			'one' => 'hello',
			'two' => 'world',
			'ten' => '',
		);

		$validation = Validation::factory($data);

		$this->assertSame($data, $validation->as_array());
	}

	/**
	 * Tests Validation::data()
	 *
	 * @test
	 * @covers Validation::data
	 */
	public function test_data_returns_original_array()
	{
		$data = array(
			'one' => 'hello',
			'two' => 'world',
			'ten' => '',
		);

		$validation = Validation::factory($data);

		$this->assertSame($data, $validation->data());
	}

	// @codingStandardsIgnoreStart
	public function test_offsetExists()
	// @codingStandardsIgnoreEnd
	{
		$array = array(
			'one' => 'Hello',
			'two' => 'World',
			'ten' => NULL,
		);

		$validation = Validation::factory($array);

		$this->assertTrue(isset($validation['one']));
		$this->assertFalse(isset($validation['ten']));
		$this->assertFalse(isset($validation['five']));
	}

	// @codingStandardsIgnoreStart
	public function test_offsetSet_throws_exception()
	// @codingStandardsIgnoreEnd
	{
		$this->setExpectedException('Kohana_Exception');

		$validation = Validation::factory(array());

		// Validation is read-only
		$validation['field'] = 'something';
	}

	// @codingStandardsIgnoreStart
	public function test_offsetGet()
	// @codingStandardsIgnoreEnd
	{
		$array = array(
			'one' => 'Hello',
			'two' => 'World',
			'ten' => NULL,
		);

		$validation = Validation::factory($array);

		$this->assertSame($array['one'], $validation['one']);
		$this->assertSame($array['two'], $validation['two']);
		$this->assertSame($array['ten'], $validation['ten']);
	}

	// @codingStandardsIgnoreStart
	public function test_offsetUnset()
	// @codingStandardsIgnoreEnd
	{
		$this->setExpectedException('Kohana_Exception');

		$validation = Validation::factory(array(
			'one' => 'Hello, World!',
		));

		// Validation is read-only
		unset($validation['one']);
	}

	/**
	 * http://dev.kohanaframework.org/issues/4365
	 *
	 * @test
	 * @covers Validation::errors
	 */
	public function test_error_type_check()
	{
		$array = array(
			'email' => 'not an email address',
		);

		$validation = Validation::factory($array)
			->rule('email', 'not_empty')
			->rule('email', 'email')
			;

		$validation->check();

		$errors = $validation->errors('tests/validation/error_type_check');

		$this->assertSame($errors, $validation->errors('validation'));
	}

}
