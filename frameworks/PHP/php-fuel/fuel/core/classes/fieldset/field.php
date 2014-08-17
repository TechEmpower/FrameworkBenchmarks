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
 * Fieldset Class
 *
 * Define a set of fields that can be used to generate a form or to validate input.
 *
 * @package   Fuel
 * @category  Core
 */
class Fieldset_Field
{
	/**
	 * @var  Fieldset  Fieldset this field belongs to
	 */
	protected $fieldset;

	/**
	 * @var  string  Name of this field
	 */
	protected $name = '';

	/**
	 * @var  string  Base name of this field
	 */
	protected $basename = '';

	/**
	 * @var  string  Field type for form generation, false to prevent it showing
	 */
	protected $type = 'text';

	/**
	 * @var  string  Field label for validation errors and form label generation
	 */
	protected $label = '';

	/**
	 * @var  mixed  (Default) value of this field
	 */
	protected $value;

	/**
	 * @var  string  Description text to show with the field
	 */
	protected $description = '';

	/**
	 * @var  array  Rules for validation
	 */
	protected $rules = array();

	/**
	 * @var  array  Attributes for form generation
	 */
	protected $attributes = array();

	/**
	 * @var  array  Options, only available for select, radio & checkbox types
	 */
	protected $options = array();

	/**
	 * @var  string  Template for form building
	 */
	protected $template;

	/**
	 * @var  array  overwrites for default error messages
	 */
	protected $error_messages = array();

	/**
	 * Constructor
	 *
	 * @param  string
	 * @param  string
	 * @param  array
	 * @param  array
	 * @param  Fieldset
	 */
	public function __construct($name, $label = '', array $attributes = array(), array $rules = array(), $fieldset = null)
	{
		$this->name = (string) $name;

		// determine the field's base name (for fields with array indices)
		$this->basename = ($pos = strpos($this->name, '[')) ? rtrim(substr(strrchr($this->name, '['), 1), ']') : $this->name;

		$this->fieldset = $fieldset instanceof Fieldset ? $fieldset : null;

		// Don't allow name in attributes
		unset($attributes['name']);

		// Take rules out of attributes
		unset($attributes['rules']);

		// Use specific setter when available
		foreach ($attributes as $attr => $val)
		{
			if (method_exists($this, $method = 'set_'.$attr))
			{
				$this->{$method}($val);
				unset($attributes[$attr]);
			}
		}

		// Add default "type" attribute if not specified
		empty($attributes['type']) and $this->set_type($this->type);

		// only when non-empty, will supersede what was given in $attributes
		$label and $this->set_label($label);

		$this->attributes = array_merge($this->attributes, $attributes);

		foreach ($rules as $rule)
		{
			call_user_func_array(array($this, 'add_rule'), (array) $rule);
		}
	}

	/**
	 * @param   Fieldset  Fieldset to assign the field to
	 * @return  Fieldset_Field
	 * @throws  \RuntimeException
	 */
	public function set_fieldset(Fieldset $fieldset)
	{
		if ($this->fieldset)
		{
			throw new \RuntimeException('Field already belongs to a fieldset, cannot be reassigned.');
		}

		$this->fieldset = $fieldset;

		return $this;
	}

	/**
	 * Change the field label
	 *
	 * @param   string
	 * @return  Fieldset_Field  this, to allow chaining
	 */
	public function set_label($label)
	{
		$this->label = $label;
		$this->set_attribute('label', $label);

		return $this;
	}

	/**
	 * Change the field type for form generation
	 *
	 * @param   string
	 * @return  Fieldset_Field  this, to allow chaining
	 */
	public function set_type($type)
	{
		$this->type = $type;
		$this->set_attribute('type', $type);

		return $this;
	}

	/**
	 * Change the field's current or default value
	 *
	 * @param   string
	 * @param   bool
	 * @return  Fieldset_Field  this, to allow chaining
	 */
	public function set_value($value, $repopulate = false)
	{
		// Repopulation is handled slightly different in some cases
		if ($repopulate)
		{
			if (($this->type == 'radio' or $this->type == 'checkbox') and empty($this->options))
			{
				if ($this->value == $value)
				{
					$this->set_attribute('checked', 'checked');
				}

				return $this;
			}
		}

		$this->value = $value;
		$this->set_attribute('value', $value);

		return $this;
	}

	/**
	 * Change the field description
	 *
	 * @param   string
	 * @return  Fieldset_Field  this, to allow chaining
	 */
	public function set_description($description)
	{
		$this->description = strval($description);

		return $this;
	}

	/**
	 * Template the output
	 *
	 * @param   string
	 * @return  Fieldset_Field  this, to allow chaining
	 */
	public function set_template($template = null)
	{
		$this->template = $template;

		return $this;
	}

	/**
	 * Overwrite a default error message
	 *
	 * @param   string  $rule
	 * @param   string  $msg
	 * @return  Fieldset_Field
	 */
	public function set_error_message($rule, $msg)
	{
		empty($rule) and $rule = 0;
		$this->error_messages[$rule] = strval($msg);

		return $this;
	}

	/**
	 * Check if a rule has an error message overwrite
	 *
	 * @param   string  $rule
	 * @return  null|string
	 */
	public function get_error_message($rule)
	{
		if (isset($this->error_messages[$rule]))
		{
			return $this->error_messages[$rule];
		}
		elseif (isset($this->error_messages[0]))
		{
			return $this->error_messages[0];
		}

		return null;
	}

	/**
	 * Add a validation rule
	 * any further arguements after the callback will be used as arguements for the callback
	 *
	 * @param   string|Callback	either a validation rule or full callback
	 * @return  Fieldset_Field  this, to allow chaining
	 */
	public function add_rule($callback)
	{
		$args = array_slice(func_get_args(), 1);
		$this->rules[] = array($callback, $args);

		// Set required setting for forms when rule was applied
		if ($callback === 'required')
		{
			$this->set_attribute('required', 'required');
		}

		return $this;
	}

	/**
	 * Delete a validation rule
	 *
	 * @param   string|Callback	either a validation rule or full callback
	 * @param   bool	whether to also reset related attributes
	 * @return  Fieldset_Field  this, to allow chaining
	 */
	public function delete_rule($callback, $set_attr = true)
	{
		foreach($this->rules as $index => $rule)
		{
			if ($rule[0] === $callback)
			{
				unset($this->rules[$index]);
				break;
			}
		}

		if ($callback === 'required' and $set_attr)
		{
			unset($this->attributes[$callback]);
		}

		return $this;
	}

	/**
	 * Sets an attribute on the field
	 *
	 * @param   string
	 * @param   mixed   new value or null to unset
	 * @return  Fieldset_Field  this, to allow chaining
	 */
	public function set_attribute($config, $value = null)
	{
		$config = is_array($config) ? $config : array($config => $value);
		foreach ($config as $key => $value)
		{
			if ($value === null)
			{
				unset($this->attributes[$key]);
			}
			else
			{
				$this->attributes[$key] = $value;
			}
		}

		return $this;
	}

	/**
	 * Get a single or multiple attributes by key
	 *
	 * @param   string|array  a single key or multiple in an array, empty to fetch all
	 * @param   mixed         default output when attribute wasn't set
	 * @return  mixed|array   a single attribute or multiple in an array when $key input was an array
	 */
	public function get_attribute($key = null, $default = null)
	{
		if ($key === null)
		{
			return $this->attributes;
		}

		if (is_array($key))
		{
			$output = array();
			foreach ($key as $k)
			{
				$output[$k] = array_key_exists($k, $this->attributes) ? $this->attributes[$k] : $default;
			}
			return $output;
		}

		return array_key_exists($key, $this->attributes) ? $this->attributes[$key] : $default;
	}

	/**
	 * Add an option value with label
	 *
	 * @param   string|array  one option value, or multiple value=>label pairs in an array
	 * @param   string
	 * @param   bool            Whether or not to replace the current options
	 * @return  Fieldset_Field  this, to allow chaining
	 */
	public function set_options($value, $label = null, $replace_options = false)
	{
		if ( ! is_array($value))
		{
			\Arr::set($this->options, $value, $label);
			return $this;
		}

		$merge = function(&$array, $new, $merge)
		{
			foreach ($new as $k => $v)
			{
				if (isset($array[$k]) and is_array($array[$k]) and is_array($v))
				{
					$merge($array[$k], $v);
				}
				else
				{
					$array[$k] = $v;
				}
			}
		};

		($replace_options or empty($this->options)) ? $this->options = $value : $merge($this->options, $value, $merge);

		return $this;
	}

	/**
	 * Magic get method to allow getting class properties but still having them protected
	 * to disallow writing.
	 *
	 * @return  mixed
	 */
	public function __get($property)
	{
		return $this->$property;
	}

	/**
	 * Build the field
	 *
	 * @return  string
	 */
	public function __toString()
	{
		try
		{
			return $this->build();
		}
		catch (\Exception $e)
		{
			return $e->getMessage();
		}
	}

	/**
	 * Return the parent Fieldset object
	 *
	 * @return  Fieldset
	 */
	public function fieldset()
	{
		return $this->fieldset;
	}

	/**
	 * Alias for $this->fieldset->add() to allow chaining
	 *
	 * @return Fieldset_Field
	 */
	public function add($name, $label = '', array $attributes = array(), array $rules = array())
	{
		return $this->fieldset()->add($name, $label, $attributes, $rules);
	}

	/**
	 * Alias for $this->fieldset->add_before() to allow chaining
	 *
	 * @return Fieldset_Field
	 */
	public function add_before($name, $label = '', array $attributes = array(), array $rules = array(), $fieldname = null)
	{
		return $this->fieldset()->add_before($name, $label, $attributes, $rules, $fieldname);
	}

	/**
	 * Alias for $this->fieldset->add_after() to allow chaining
	 *
	 * @return Fieldset_Field
	 */
	public function add_after($name, $label = '', array $attributes = array(), array $rules = array(), $fieldname = null)
	{
		return $this->fieldset()->add_after($name, $label, $attributes, $rules, $fieldname);
	}

	/**
	 * Build the field
	 *
	 * @return  string
	 */
	public function build()
	{
		$form = $this->fieldset()->form();

		// Add IDs when auto-id is on
		if ($form->get_config('auto_id', false) === true and $this->get_attribute('id') == '')
		{
			$auto_id = $form->get_config('auto_id_prefix', '')
				.str_replace(array('[', ']'), array('-', ''), $this->name);
			$this->set_attribute('id', $auto_id);
		}

		switch( ! empty($this->attributes['tag']) ? $this->attributes['tag'] : $this->type)
		{
			case 'hidden':
				$build_field = $form->hidden($this->name, $this->value, $this->attributes);
				break;
			case 'radio': case 'checkbox':
				if ($this->options)
				{
					$build_field = array();
					$i = 0;
					foreach ($this->options as $value => $label)
					{
						$attributes = $this->attributes;
						$attributes['name'] = $this->name;
						$this->type == 'checkbox' and $attributes['name'] .= '['.$i.']';

						$attributes['value'] = $value;
						$attributes['label'] = $label;

						if (is_array($this->value) ? in_array($value, $this->value) : $value == $this->value)
						{
							$attributes['checked'] = 'checked';
						}

						if( ! empty($attributes['id']))
						{
							$attributes['id'] .= '_'.$i;
						}
						else
						{
							$attributes['id'] = null;
						}
						$build_field[$form->label($label, null, array('for' => $attributes['id']))] = $this->type == 'radio'
							? $form->radio($attributes)
							: $form->checkbox($attributes);

						$i++;
					}
				}
				else
				{
					$build_field = $this->type == 'radio'
						? $form->radio($this->name, $this->value, $this->attributes)
						: $form->checkbox($this->name, $this->value, $this->attributes);
				}
				break;
			case 'select':
				$attributes = $this->attributes;
				$name = $this->name;
				unset($attributes['type']);
				array_key_exists('multiple', $attributes) and $name .= '[]';
				$build_field = $form->select($name, $this->value, $this->options, $attributes);
				break;
			case 'textarea':
				$attributes = $this->attributes;
				unset($attributes['type']);
				$build_field = $form->textarea($this->name, $this->value, $attributes);
				break;
			case 'button':
				$build_field = $form->button($this->name, $this->value, $this->attributes);
				break;
			case false:
				$build_field = '';
				break;
			default:
				$build_field = $form->input($this->name, $this->value, $this->attributes);
				break;
		}

		if (empty($build_field) or $this->type == 'hidden')
		{
			return $build_field;
		}

		return $this->template($build_field);
	}

	protected function template($build_field)
	{
		$form = $this->fieldset()->form();

		$required_mark = $this->get_attribute('required', null) ? $form->get_config('required_mark', null) : null;
		$label = $this->label ? $form->label($this->label, null, array('id' => 'label_'.$this->name, 'for' => $this->get_attribute('id', null))) : '';
		$error_template = $form->get_config('error_template', '');
		$error_msg = ($form->get_config('inline_errors') && $this->error()) ? str_replace('{error_msg}', $this->error(), $error_template) : '';
		$error_class = $this->error() ? $form->get_config('error_class') : '';

		if (is_array($build_field))
		{
			$label = $this->label ? $form->label($this->label) : '';
			$template = $this->template ?: $form->get_config('multi_field_template', "\t\t<tr>\n\t\t\t<td class=\"{error_class}\">{group_label}{required}</td>\n\t\t\t<td class=\"{error_class}\">{fields}\n\t\t\t\t{field} {label}<br />\n{fields}\t\t\t{error_msg}\n\t\t\t</td>\n\t\t</tr>\n");
			if ($template && preg_match('#\{fields\}(.*)\{fields\}#Dus', $template, $match) > 0)
			{
				$build_fields = '';
				foreach ($build_field as $lbl => $bf)
				{
					$bf_temp = str_replace('{label}', $lbl, $match[1]);
					$bf_temp = str_replace('{required}', $required_mark, $bf_temp);
					$bf_temp = str_replace('{field}', $bf, $bf_temp);
					$build_fields .= $bf_temp;
				}

				$template = str_replace($match[0], '{fields}', $template);
				$template = str_replace(array('{group_label}', '{required}', '{fields}', '{error_msg}', '{error_class}', '{description}'), array($label, $required_mark, $build_fields, $error_msg, $error_class, $this->description), $template);

				return $template;
			}

			// still here? wasn't a multi field template available, try the normal one with imploded $build_field
			$build_field = implode(' ', $build_field);
		}

		// determine the field_id, which allows us to identify the field for CSS purposes
		$field_id = 'col_'.$this->name;
		if ($parent = $this->fieldset()->parent())
		{
			$parent->get_tabular_form() and $field_id = $parent->get_tabular_form().'_col_'.$this->basename;
		}

		$template = $this->template ?: $form->get_config('field_template', "\t\t<tr>\n\t\t\t<td class=\"{error_class}\">{label}{required}</td>\n\t\t\t<td class=\"{error_class}\">{field} {description} {error_msg}</td>\n\t\t</tr>\n");
		$template = str_replace(array('{label}', '{required}', '{field}', '{error_msg}', '{error_class}', '{description}', '{field_id}'),
			array($label, $required_mark, $build_field, $error_msg, $error_class, $this->description, $field_id),
			$template);

		return $template;
	}

	/**
	 * Alias for $this->fieldset->validation->input() for this field
	 *
	 * @return  mixed
	 */
	public function input()
	{
		return $this->fieldset()->validation()->input($this->name);
	}

	/**
	 * Alias for $this->fieldset->validation->validated() for this field
	 *
	 * @return  mixed
	 */
	public function validated()
	{
		return $this->fieldset()->validation()->validated($this->name);
	}

	/**
	 * Alias for $this->fieldset->validation->error() for this field
	 *
	 * @return  Validation_Error
	 */
	public function error()
	{
		return $this->fieldset()->validation()->error($this->name);
	}
}
