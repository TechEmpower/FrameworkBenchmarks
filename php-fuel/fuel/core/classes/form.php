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
 * Form Class
 *
 * Helper for creating forms with support for creating dynamic form objects.
 *
 * @package   Fuel
 * @category  Core
 */
class Form
{
	/*
	 * @var  Form_Instance  the default form instance
	 */
	protected static $instance;

	/**
	 * When autoloaded this will method will be fired, load once and once only
	 *
	 * @return  void
	 */
	public static function _init()
	{
		\Config::load('form', true);

		static::$instance = static::forge('_default_', \Config::get('form'));
	}

	public static function forge($fieldset = 'default', array $config = array())
	{
		if (is_string($fieldset))
		{
			($set = \Fieldset::instance($fieldset)) and $fieldset = $set;
		}

		if ($fieldset instanceof Fieldset)
		{
			if ($fieldset->form(false) != null)
			{
				throw new \DomainException('Form instance already exists, cannot be recreated. Use instance() instead of forge() to retrieve the existing instance.');
			}
		}

		return new \Form_Instance($fieldset, $config);
	}

	/**
	 * Returns the 'default' instance of Form
	 *
	 * @param   null|string  $name
	 * @return  Form_Instance
	 */
	public static function instance($name = null)
	{
		$fieldset = \Fieldset::instance($name);
		return $fieldset === false ? false : $fieldset->form();
	}

	/**
	 * Create a form open tag
	 *
	 * @param   string|array  action string or array with more tag attribute settings
	 * @return  string
	 */
	public static function open($attributes = array(), array $hidden = array())
	{
		return static::$instance->open($attributes, $hidden);
	}

	/**
	 * Create a form close tag
	 *
	 * @return  string
	 */
	public static function close()
	{
		return static::$instance->close();
	}

	/**
	 * Create a fieldset open tag
	 *
	 * @param   array   array with tag attribute settings
	 * @param   string  string for the fieldset legend
	 * @return  string
	 */
	public static function fieldset_open($attributes = array(), $legend = null)
	{
		return static::$instance->fieldset_open($attributes, $legend);
	}

	/**
	 * Create a fieldset close tag
	 *
	 * @return string
	 */
	public static function fieldset_close()
	{
		return static::$instance->fieldset_close();
	}

	/**
	 * Create a form input
	 *
	 * @param   string|array  either fieldname or full attributes array (when array other params are ignored)
	 * @param   string
	 * @param   array
	 * @return  string
	 */
	public static function input($field, $value = null, array $attributes = array())
	{
		return static::$instance->input($field, $value, $attributes);
	}

	/**
	 * Create a hidden field
	 *
	 * @param   string|array  either fieldname or full attributes array (when array other params are ignored)
	 * @param   string
	 * @param   array
	 * @return  string
	 */
	public static function hidden($field, $value = null, array $attributes = array())
	{
		return static::$instance->hidden($field, $value, $attributes);
	}

	/**
	 * Create a password input field
	 *
	 * @param   string|array  either fieldname or full attributes array (when array other params are ignored)
	 * @param   string
	 * @param   array
	 * @return  string
	 */
	public static function password($field, $value = null, array $attributes = array())
	{
		return static::$instance->password($field, $value, $attributes);
	}

	/**
	 * Create a radio button
	 *
	 * @param   string|array  either fieldname or full attributes array (when array other params are ignored)
	 * @param   string
	 * @param   mixed         either attributes (array) or bool/string to set checked status
	 * @param   array
	 * @return  string
	 */
	public static function radio($field, $value = null, $checked = null, array $attributes = array())
	{
		return static::$instance->radio($field, $value, $checked, $attributes);
	}

	/**
	 * Create a checkbox
	 *
	 * @param   string|array  either fieldname or full attributes array (when array other params are ignored)
	 * @param   string
	 * @param   mixed         either attributes (array) or bool/string to set checked status
	 * @param   array
	 * @return  string
	 */
	public static function checkbox($field, $value = null, $checked = null, array $attributes = array())
	{
		return static::$instance->checkbox($field, $value, $checked, $attributes);
	}

	/**
	 * Create a file upload input field
	 *
	 * @param   string|array  either fieldname or full attributes array (when array other params are ignored)
	 * @param   array
	 * @return  string
	 */
	public static function file($field, array $attributes = array())
	{
		return static::$instance->file($field, $attributes);
	}

	/**
	 * Create a button
	 *
	 * @param   string|array  either fieldname or full attributes array (when array other params are ignored)
	 * @param   string
	 * @param   array
	 * @return  string
	 */
	public static function button($field, $value = null, array $attributes = array())
	{
		return static::$instance->button($field, $value, $attributes);
	}

	/**
	 * Create a reset button
	 *
	 * @param   string|array  either fieldname or full attributes array (when array other params are ignored)
	 * @param   string
	 * @param   array
	 * @return  string
	 */
	public static function reset($field = 'reset', $value = 'Reset', array $attributes = array())
	{
		return static::$instance->reset($field, $value, $attributes);
	}

	/**
	 * Create a submit button
	 *
	 * @param   string|array  either fieldname or full attributes array (when array other params are ignored)
	 * @param   string
	 * @param   array
	 * @return  string
	 */
	public static function submit($field = 'submit', $value = 'Submit', array $attributes = array())
	{
		return static::$instance->submit($field, $value, $attributes);
	}

	/**
	 * Create a textarea field
	 *
	 * @param   string|array  either fieldname or full attributes array (when array other params are ignored)
	 * @param   string
	 * @param   array
	 * @return  string
	 */
	public static function textarea($field, $value = null, array $attributes = array())
	{
		return static::$instance->textarea($field, $value, $attributes);
	}

	/**
	 * Select
	 *
	 * Generates a html select element based on the given parameters
	 *
	 * @param   string|array  either fieldname or full attributes array (when array other params are ignored)
	 * @param   string  selected value(s)
	 * @param   array   array of options and option groups
	 * @param   array
	 * @return  string
	 */
	public static function select($field, $values = null, array $options = array(), array $attributes = array())
	{
		return static::$instance->select($field, $values, $options, $attributes);
	}

	/**
	 * Create a label field
	 *
	 * @param   string|array  either fieldname or full attributes array (when array other params are ignored)
	 * @param   string
	 * @param   array
	 * @return  string
	 */
	public static function label($label, $id = null, array $attributes = array())
	{
		return static::$instance->label($label, $id, $attributes);
	}

	/**
	 * Prep Value
	 *
	 * Prepares the value for display in the form
	 *
	 * @param   string
	 * @return  string
	 */
	public static function prep_value($value)
	{
		return static::$instance->prep_value($value);
	}

	/**
	 * Attr to String
	 *
	 * Wraps the global attributes function and does some form specific work
	 *
	 * @param   array  $attr
	 * @return  string
	 */
	protected static function attr_to_string($attr)
	{
		return static::$instance->attr_to_string($attr);
	}

}
