<?php
/**
 * Form
 *
 * Create HTML forms from a validation object using method chaining.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

class Form
{
	// Array of field objects
	protected $fields;

	protected $field;
	protected $validation;
	protected $attributes;
	protected $value;
	protected $label;
	protected $type = 'text';
	protected $options;
	protected $tag;

	/**
	 * Create an HTML form containing the given fields
	 *
	 * @param object $validation object
	 * @param string $field name
	 */
	public function __construct($validation = NULL, $field = NULL)
	{
		$this->validation = $validation;
		$this->field = $field;

		if($field)
		{
			$this->attributes = array('name' => $this->field, 'id' => $this->field);
		}
	}


	/**
	 * Add an array of attributes to the form element
	 *
	 * @param array $attributes to add
	 */
	public function attributes(array $attributes)
	{
		foreach($attributes as $key => $value)
		{
			$this->attributes[$key] = $value;
		}
		return $this;
	}


	/**
	 * Load a new instance of this object for the given field
	 */
	public function __get($field)
	{
		if(empty($this->fields[$field]))
		{
			$this->fields[$field] = new $this($this->validation, $field, $this->attributes);
		}
		return $this->fields[$field];
	}


	/**
	 * Set the field value
	 *
	 * @param mixed $value
	 */
	public function value($value)
	{
		$this->value = $value;
		return $this;
	}


	/**
	 * Add a form label before the given element
	 *
	 * @param string $label text
	 */
	public function label($label)
	{
		$this->label = $label;
		return $this;
	}


	/**
	 * Set the form element to display as a selectbox
	 *
	 * @param array $options of select box
	 */
	public function select(array $options)
	{
		$this->type = 'select';
		$this->options = $options;
		return $this;
	}

	/**
	 * Set the form element to display as an input box
	 *
	 * @param string $type of input (text, password, hidden...)
	 */
	public function input($type)
	{
		$this->type = $type;
		return $this;
	}


	/**
	 * Set the form element to display as a textarea
	 */
	public function textarea()
	{
		$this->type = 'textarea';
		return $this;
	}


	/**
	 * Wrap the given form element in this tag
	 *
	 * @param string $tag name
	 */
	public function wrap($tag)
	{
		$this->tag = $tag;
		return $this;
	}


	/**
	 * Return the current HTML form as a string
	 */
	public function __toString()
	{
		try
		{
			if($this->field)
			{
				return $this->render_field();
			}

			if( ! $this->fields) return '';

			$output = '';

			foreach($this->fields as $field) $output .= $field;

			return $output;
		}
		catch(\Exception $e)
		{
			Error::exception($e);
			return '';
		}
	}


	/**
	 * Render the given field
	 *
	 * @return string
	 */
	protected function render_field()
	{
		$html = "\n";

		if( ! $this->attributes)
		{
			$this->attributes = array();
		}

		// Configure the attributes
		$attributes = $this->attributes;

		// Get the current value
		if($this->value !== NULL)
		{
			$value = $this->value;
		}
		else
		{
			$value = $this->validation->value($this->field);
		}

		if($this->label)
		{
			$html .= '<label for="'. $this->field . '">' . $this->label . "</label>";
		}

		if($this->type == 'select')
		{
			$html .= \Micro\HTML::select($this->field, $this->options, $value, $attributes);
		}
		elseif($this->type == 'textarea')
		{
			$html .= \Micro\HTML::tag('textarea', $value, $attributes);
		}
		else
		{
			// Input field
			$attributes = $attributes + array('type' => $this->type, 'value' => $value);

			$html .= \Micro\HTML::tag('input', FALSE, $attributes);
		}

		// If there was a validation error
		if($error = $this->validation->error($this->field))
		{
			if(isset($attributes['class']))
			{
				$attributes['class'] .= ' error';
			}
			else
			{
				$attributes['class'] = $this->field . ' ' . $this->type . ' error';
			}

			$html .= "\n<div class=\"error_message\">$error</div>";
		}

		if($this->tag)
		{
			$html = \Micro\HTML::tag($this->tag, $html . "\n") . "\n";
		}

		return $html;
	}

}

// END
