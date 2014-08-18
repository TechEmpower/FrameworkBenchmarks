<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Fuel\Core;

/**
 * Validation class tests
 *
 * @group Core
 * @group Validation
 */
class Test_Validation extends TestCase
{
	public static function form_provider()
	{
		return array(
			array(
				array(
					'foo' => 'bar',
					'bar' => 'foo',
					'boo' => 'bar',
					'cat' => '',
					'dog' => '245abc',
					'php' => 1,
					'sky' => 'blue',
					'one' => 'john@doe.com',
					'two' => 'john@doe.com, jane@doe.com',
					'owt' => 'john@doe.com; jane@doe.com',
					'six' => 6,
					'ten' => 10,
					'dns' => '127.0.0.1',
					'snd' => '127.0.0',
					'url' => 'http://www.google.com',
				)
			)
		);
	}

	/**
	 * Validation:  required
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_required_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('foo', 'Foo', 'required');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  required
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_required_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('cat', 'Cat', 'required');
		$val->run($input);

		$output = $val->error('cat', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  match_value
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_match_value_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('foo', 'Foo', 'match_value[bar]');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  match_value
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_match_value_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('foo', 'Foo', "match_value['foo']");
		$val->run($input);

		$output = $val->error('foo', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  match_value (strict)
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_match_value_strict_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('php', 'PHP', '')->add_rule('match_value', 1, 1);

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  match_value (strict)
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_match_value_strict_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('php', 'PHP', '')->add_rule('match_value', '1', 1);
		$val->run($input);

		$output = $val->error('php', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  match_pattern
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_match_pattern_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('dog', 'Dog', '')->add_rule('match_pattern', '/\d{3}\w{3}/');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  match_pattern
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_match_pattern_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('dog', 'Dog', '')->add_rule('match_pattern', '/^\d{2}[abc]{3}/');
		$val->run($input);

		$output = $val->error('dog', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  match_field
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_match_field_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('foo', 'Foo', 'valid_string');
		$val->add_field('boo', 'Boo', 'match_field[foo]');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  match_field
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_match_field_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('bar', 'Bar', 'valid_string');
		$val->add_field('boo', 'Boo', 'match_field[bar]');
		$val->run($input);

		$output = $val->error('boo', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  min_length
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_min_length_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('boo', 'Boo', 'min_length[2]');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  min_length
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_min_length_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('boo', 'Boo', 'min_length[4]');
		$val->run($input);

		$output = $val->error('boo', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  max_length
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_max_length_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('boo', 'Boo', 'max_length[4]');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  max_length
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_max_length_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('boo', 'Boo', 'max_length[2]');
		$val->run($input);

		$output = $val->error('boo', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  exact_length
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_exact_length_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('boo', 'Boo', 'exact_length[3]');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  exact_length
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_exact_length_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('boo', 'Boo', 'max_length[2]');
		$val->run($input);

		$output = $val->error('boo', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  valid_email
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_valid_email_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('one', 'Email', 'valid_email');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  valid_email
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_valid_email_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('boo', 'Email', 'valid_email');
		$val->run($input);

		$output = $val->error('boo', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  valid_emails
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_valid_emails_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('two', 'Emails', 'valid_emails');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  valid_emails (different separator)
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_valid_emails_separator_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add('owt', 'Emails')->add_rule('valid_emails', ';');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  valid_emails
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_valid_emails_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('boo', 'Email', 'valid_emails');
		$val->run($input);

		$output = $val->error('boo', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  valid_url
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_valid_url_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('url', 'Url', 'valid_url');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  valid_url
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_valid_url_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('boo', 'Url', 'valid_url');
		$val->run($input);

		$output = $val->error('boo', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  valid_ip
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_valid_ip_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('dns', 'IP', 'valid_ip');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  valid_ip
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_valid_ip_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('snd', 'IP', 'valid_ip');
		$val->run($input);

		$output = $val->error('snd', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  numeric_min
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_numeric_min_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('six', 'Number', 'numeric_min[4]');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  numeric_min
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_numeric_min_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('six', 'Number', 'numeric_min[8]');
		$val->run($input);

		$output = $val->error('six', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  numeric_max
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_numeric_max_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('ten', 'Number', 'numeric_max[11]');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  numeric_max
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_numeric_max_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('ten', 'Number', 'numeric_max[8]');
		$val->run($input);

		$output = $val->error('ten', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  numeric_between
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_numeric_between_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('ten', 'Number', 'numeric_between[9,11]');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  numeric_between
	 * Expecting:   failure
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_numeric_between_failure($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('ten', 'Number', 'numeric_between[7,8]');
		$val->run($input);

		$output = $val->error('ten', false) ? true : false;
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  required_with
	 * Expecting:   success
	 *
	 * @dataProvider    form_provider
	 */
	public function test_validation_required_with_success($input)
	{
		$val = Validation::forge(__FUNCTION__);
		$val->add_field('foo', 'Foo', 'valid_string');
		$val->add_field('bar', 'Bar', 'required_with[foo]');

		$output = $val->run($input);
		$expected = true;

		$this->assertEquals($expected, $output);
	}

	/**
	 * Validation:  valid_string numeric
	 * Expecting:   success
	 */
	public function test_validation_valid_string_numeric_success()
	{
		$post = array(
			'f1' => '123456',
		);

		$val = Validation::forge(__FUNCTION__);
		$val->add('f1', 'F1')->add_rule('valid_string', 'numeric');
		$test = $val->run($post);
		$expected = true;

		$this->assertEquals($expected, $test);
	}

	/**
	* Validation:  valid_string numeric
	* Expecting:   failure
	*/
	public function test_validation_valid_string_numeric_failure()
	{
		$post = array(
			'f1' => 'a123456',
		);

		$val = Validation::forge(__FUNCTION__);
		$val->add('f1', 'F1')->add_rule('valid_string', 'numeric');
		$test = $val->run($post);
		$expected = false;

		$this->assertEquals($expected, $test);
	}

	/**
	 * Validation:  valid_string alpha,numeric
	 * Expecting:   failure
	 */
	public function test_validation_valid_string_multiple_flags_error_message_add_rule() {
		$post = array(
			'f1' => '123 abc',
		);

		$val = Validation::forge(__FUNCTION__);
		$val->add('f1', 'F1')->add_rule('valid_string', array('alpha', 'numeric'));
		$val->set_message('valid_string', 'The valid string rule :rule(:param:1) failed for field :label');
		$val->run($post);

		$test = $val->error('f1')->get_message();
		$expected = 'The valid string rule valid_string(alpha, numeric) failed for field F1';

		$this->assertEquals($expected, $test);
	}

	/**
	 * Validation:  valid_string alpha,numeric
	 * Expecting:   failure
	 */
	public function test_validation_valid_string_multiple_flags_error_message_add_field() {
		$post = array(
			'f1' => '123 abc',
		);

		$val = Validation::forge(__FUNCTION__);
		$val->add_field('f1', 'F1', 'valid_string[alpha,numeric]');
		$val->set_message('valid_string', 'The valid string rule :rule(:param:1) failed for field :label');
		$val->run($post);

		$test = $val->error('f1')->get_message();
		$expected = 'The valid string rule valid_string(alpha, numeric) failed for field F1';

		$this->assertEquals($expected, $test);
	}
}
