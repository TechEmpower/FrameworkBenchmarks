<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Tests Kohana Form helper
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.form
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @author     Jeremy Bush <contractfrombelow@gmail.com>
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_FormTest extends Unittest_TestCase
{
	/**
	 * Defaults for this test
	 * @var array
	 */
	// @codingStandardsIgnoreStart
	protected $environmentDefault = array(
		'Kohana::$base_url' => '/',
		'HTTP_HOST' => 'kohanaframework.org',
		'Kohana::$index_file' => '',
	);
	// @codingStandardsIgnoreEnd

	/**
	 * Provides test data for test_open()
	 *
	 * @return array
	 */
	public function provider_open()
	{
		return array(
			array(
				  array('', NULL),
				  array('action' => '')
			),
			array(
				  array(NULL, NULL),
				  array('action' => '')
			),
			array(
				  array('foo', NULL),
				  array('action' => '/foo')
			),
			array(
				  array('foo', array('method' => 'get')),
				  array('action' => '/foo', 'method' => 'get')
			),
		);
	}

	/**
	 * Tests Form::open()
	 *
	 * @test
	 * @dataProvider provider_open
	 * @param boolean $input  Input for Form::open
	 * @param boolean $expected Output for Form::open
	 */
	public function test_open($input, $expected)
	{
		list($action, $attributes) = $input;

		$tag = Form::open($action, $attributes);

		$matcher = array(
			'tag' => 'form',
			// Default attributes
			'attributes' => array(
				'method'         => 'post',
				'accept-charset' => 'utf-8',
			),
		);

		$matcher['attributes'] = $expected + $matcher['attributes'];

		$this->assertTag($matcher, $tag);
	}

	/**
	 * Tests Form::close()
	 *
	 * @test
	 */
	public function test_close()
	{
		$this->assertSame('</form>', Form::close());
	}

	/**
	 * Provides test data for test_input()
	 *
	 * @return array
	 */
	public function provider_input()
	{
		return array(
			// $value, $result
			array('input',    'foo', 'bar', NULL, 'input'),
			array('input',    'foo',  NULL, NULL, 'input'),
			array('hidden',   'foo', 'bar', NULL, 'hidden'),
			array('password', 'foo', 'bar', NULL, 'password'),
		);
	}

	/**
	 * Tests Form::input()
	 *
	 * @test
	 * @dataProvider provider_input
	 * @param boolean $input  Input for Form::input
	 * @param boolean $expected Output for Form::input
	 */
	public function test_input($type, $name, $value, $attributes)
	{
		$matcher = array(
			'tag' => 'input',
			'attributes' => array('name' => $name, 'type' => $type)
		);

		// Form::input creates a text input
		if ($type === 'input')
		{
			$matcher['attributes']['type'] = 'text';
		}

		// NULL just means no value
		if ($value !== NULL)
		{
			$matcher['attributes']['value'] = $value;
		}

		// Add on any attributes
		if (is_array($attributes))
		{
			$matcher['attributes'] = $attributes + $matcher['attributes'];
		}

		$tag = Form::$type($name, $value, $attributes);

		$this->assertTag($matcher, $tag, $tag);
	}

	/**
	 * Provides test data for test_file()
	 *
	 * @return array
	 */
	public function provider_file()
	{
		return array(
			// $value, $result
			array('foo', NULL, '<input type="file" name="foo" />'),
		);
	}

	/**
	 * Tests Form::file()
	 *
	 * @test
	 * @dataProvider provider_file
	 * @param boolean $input  Input for Form::file
	 * @param boolean $expected Output for Form::file
	 */
	public function test_file($name, $attributes, $expected)
	{
		$this->assertSame($expected, Form::file($name, $attributes));
	}

	/**
	 * Provides test data for test_check()
	 *
	 * @return array
	 */
	public function provider_check()
	{
		return array(
			// $value, $result
			array('checkbox', 'foo', NULL, FALSE, NULL),
			array('checkbox', 'foo', NULL, TRUE, NULL),
			array('checkbox', 'foo', 'bar', TRUE, NULL),

			array('radio', 'foo', NULL, FALSE, NULL),
			array('radio', 'foo', NULL, TRUE, NULL),
			array('radio', 'foo', 'bar', TRUE, NULL),
		);
	}

	/**
	 * Tests Form::check()
	 *
	 * @test
	 * @dataProvider provider_check
	 * @param boolean $input  Input for Form::check
	 * @param boolean $expected Output for Form::check
	 */
	public function test_check($type, $name, $value, $checked, $attributes)
	{
		$matcher = array('tag' => 'input', 'attributes' => array('name' => $name, 'type' => $type));

		if ($value !== NULL)
		{
			$matcher['attributes']['value'] = $value;
		}

		if (is_array($attributes))
		{
			$matcher['attributes'] = $attributes + $matcher['attributes'];
		}

		if ($checked === TRUE)
		{
			$matcher['attributes']['checked'] = 'checked';
		}

		$tag = Form::$type($name, $value, $checked, $attributes);
		$this->assertTag($matcher, $tag, $tag);
	}

	/**
	 * Provides test data for test_text()
	 *
	 * @return array
	 */
	public function provider_text()
	{
		return array(
			// $value, $result
			array('textarea', 'foo', 'bar', NULL),
			array('textarea', 'foo', 'bar', array('rows' => 20, 'cols' => 20)),
			array('button', 'foo', 'bar', NULL),
			array('label', 'foo', 'bar', NULL),
			array('label', 'foo', NULL, NULL),
		);
	}

	/**
	 * Tests Form::textarea()
	 *
	 * @test
	 * @dataProvider provider_text
	 * @param boolean $input  Input for Form::textarea
	 * @param boolean $expected Output for Form::textarea
	 */
	public function test_text($type, $name, $body, $attributes)
	{
		$matcher = array(
			'tag' => $type,
			'attributes' => array(),
			'content' => $body,
		);

		if ($type !== 'label')
		{
			$matcher['attributes'] = array('name' => $name);
		}
		else
		{
			$matcher['attributes'] = array('for' => $name);
		}


		if (is_array($attributes))
		{
			$matcher['attributes'] = $attributes + $matcher['attributes'];
		}

		$tag = Form::$type($name, $body, $attributes);

		$this->assertTag($matcher, $tag, $tag);
	}


	/**
	 * Provides test data for test_select()
	 *
	 * @return array
	 */
	public function provider_select()
	{
		return array(
			// $value, $result
			array('foo', NULL, NULL, "<select name=\"foo\"></select>"),
			array('foo', array('bar' => 'bar'), NULL, "<select name=\"foo\">\n<option value=\"bar\">bar</option>\n</select>"),
			array('foo', array('bar' => 'bar'), 'bar', "<select name=\"foo\">\n<option value=\"bar\" selected=\"selected\">bar</option>\n</select>"),
			array('foo', array('bar' => array('foo' => 'bar')), NULL, "<select name=\"foo\">\n<optgroup label=\"bar\">\n<option value=\"foo\">bar</option>\n</optgroup>\n</select>"),
			array('foo', array('bar' => array('foo' => 'bar')), 'foo', "<select name=\"foo\">\n<optgroup label=\"bar\">\n<option value=\"foo\" selected=\"selected\">bar</option>\n</optgroup>\n</select>"),
			// #2286
			array('foo', array('bar' => 'bar', 'unit' => 'test', 'foo' => 'foo'), array('bar', 'foo'), "<select name=\"foo\" multiple=\"multiple\">\n<option value=\"bar\" selected=\"selected\">bar</option>\n<option value=\"unit\">test</option>\n<option value=\"foo\" selected=\"selected\">foo</option>\n</select>"),
		);
	}

	/**
	 * Tests Form::select()
	 *
	 * @test
	 * @dataProvider provider_select
	 * @param boolean $input  Input for Form::select
	 * @param boolean $expected Output for Form::select
	 */
	public function test_select($name, $options, $selected, $expected)
	{
		// Much more efficient just to assertSame() rather than assertTag() on each element
		$this->assertSame($expected, Form::select($name, $options, $selected));
	}

	/**
	 * Provides test data for test_submit()
	 *
	 * @return array
	 */
	public function provider_submit()
	{
		return array(
			// $value, $result
			array('foo', 'Foobar!', '<input type="submit" name="foo" value="Foobar!" />'),
		);
	}

	/**
	 * Tests Form::submit()
	 *
	 * @test
	 * @dataProvider provider_submit
	 * @param boolean $input  Input for Form::submit
	 * @param boolean $expected Output for Form::submit
	 */
	public function test_submit($name, $value, $expected)
	{
		$matcher = array(
			'tag' => 'input',
			'attributes' => array('name' => $name, 'type' => 'submit', 'value' => $value)
		);

		$this->assertTag($matcher, Form::submit($name, $value));
	}


	/**
	 * Provides test data for test_image()
	 *
	 * @return array
	 */
	public function provider_image()
	{
		return array(
			// $value, $result
			array('foo', 'bar', array('src' => 'media/img/login.png'), '<input type="image" name="foo" value="bar" src="/media/img/login.png" />'),
		);
	}

	/**
	 * Tests Form::image()
	 *
	 * @test
	 * @dataProvider provider_image
	 * @param boolean $name         Input for Form::image
	 * @param boolean $value        Input for Form::image
	 * @param boolean $attributes  Input for Form::image
	 * @param boolean $expected    Output for Form::image
	 */
	public function test_image($name, $value, $attributes, $expected)
	{
		$this->assertSame($expected, Form::image($name, $value, $attributes));
	}

	/**
	 * Provides test data for test_label()
	 *
	 * @return array
	 */
	function provider_label()
	{
		return array(
			// $value, $result
			// Single for provided
			array('email', NULL, NULL, '<label for="email">Email</label>'),
			array('email_address', NULL, NULL, '<label for="email_address">Email Address</label>'),
			array('email-address', NULL, NULL, '<label for="email-address">Email Address</label>'),
			// For and text values provided
			array('name', 'First name', NULL, '<label for="name">First name</label>'),
			// with attributes
			array('lastname', 'Last name', array('class' => 'text'), '<label class="text" for="lastname">Last name</label>'),
			array('lastname', 'Last name', array('class' => 'text', 'id'=>'txt_lastname'), '<label id="txt_lastname" class="text" for="lastname">Last name</label>'),
		);
	}

	/**
	 * Tests Form::label()
	 *
	 * @test
	 * @dataProvider provider_label
	 * @param boolean $for         Input for Form::label
	 * @param boolean $text        Input for Form::label
	 * @param boolean $attributes  Input for Form::label
	 * @param boolean $expected    Output for Form::label
	 */
	function test_label($for, $text, $attributes, $expected)
	{
		$this->assertSame($expected, Form::label($for, $text, $attributes));
	}
}
