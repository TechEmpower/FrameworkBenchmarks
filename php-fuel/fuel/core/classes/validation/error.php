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
 * Validation error
 *
 * Contains all the information about a validation error
 *
 * @package   Fuel
 * @category  Core
 */
class Validation_Error extends \Exception
{

	/**
	 * Load validation Language file when errors are thrown
	 */
	public static function _init()
	{
		\Lang::load('validation', true);
	}

	/**
	 * @var  Fieldset_Field  the field that caused the error
	 */
	public $field;

	/**
	 * @var  mixed  value that failed to validate
	 */
	public $value;

	/**
	 * @var  string  validation rule string representation
	 */
	public $rule;

	/**
	 * @var  array  variables passed to rule other than the value
	 */
	public $params = array();

	/**
	 * Constructor
	 *
	 * @param  array  Fieldset_Field object
	 * @param  mixed  value that failed to validate
	 * @param  array  contains rule name as key and callback as value
	 * @param  array  additional rule params
	 */
	public function __construct(Fieldset_Field $field, $value, $callback, $params)
	{
		$this->field   = $field;
		$this->value   = $value;
		$this->params  = $params;
		$this->rule    = key($callback);
	}

	/**
	 * Get Message
	 *
	 * Shows the error message which can be taken from loaded language file.
	 *
	 * @param   string  HTML to prefix error message
	 * @param   string  HTML to postfix error message
	 * @param   string  Message to use, or false to try and load it from Lang class
	 * @return  string
	 */
	public function get_message($msg = false, $open = '', $close = '')
	{
		$open   = empty($open)  ? \Config::get('validation.open_single_error', '')  : $open;
		$close  = empty($close) ? \Config::get('validation.close_single_error', '') : $close;

		if ($msg === false and ! ($msg = $this->field->get_error_message($this->rule)))
		{
			if (is_null($msg))
			{
				$msg = $this->field->fieldset()->validation()->get_message($this->rule);
			}
			if ($msg === false)
			{
				$msg = \Lang::get('validation.'.$this->rule) ?: \Lang::get('validation.'.\Arr::get(explode(':', $this->rule), 0));
			}
		}
		if ($msg == false)
		{
			return $open.'Validation rule '.$this->rule.' failed for '.$this->field->label.$close;
		}

		// only parse when there's tags in the message
		return $open.(strpos($msg, ':') === false ? $msg : $this->_replace_tags($msg)).$close;
	}

	/**
	 * Replace templating tags with values
	 *
	 * @param   error message to parse
	 * @return  string
	 */
	protected function _replace_tags($msg)
	{
		// prepare label & value
		$label    = is_array($this->field->label) ? $this->field->label['label'] : $this->field->label;
		$value    = is_array($this->value) ? implode(', ', $this->value) : $this->value;
		if (\Config::get('validation.quote_labels', false) and strpos($label, ' ') !== false)
		{
			// put the label in quotes if it contains spaces
			$label = '"'.$label.'"';
		}

		// setup find & replace arrays
		$find     = array(':field', ':label', ':value', ':rule');
		$replace  = array($this->field->name, $label, $value, $this->rule);

		// add the params to the find & replace arrays
		foreach($this->params as $key => $val)
		{
			// Convert array (as far as possible)
			if (is_array($val))
			{
				$result = '';
				foreach ($val as $v)
				{
					if (is_array($v))
					{
						$v = '(array)';
					}
					elseif (is_object($v))
					{
						$v = '(object)';
					}
					elseif (is_bool($v))
					{
						$v = $v ? 'true' : 'false';
					}
					$result .= empty($result) ? $v : (', '.$v);
				}
				$val = $result;
			}
			elseif (is_bool($val))
			{
				$val = $val ? 'true' : 'false';
			}
			// Convert object with __toString or just the classname
			elseif (is_object($val))
			{
				$val = method_exists($val, '__toString') ? (string) $val : get_class($val);
			}

			$find[]     = ':param:'.($key + 1);
			$replace[]  = $val;
		}

		// execute find & replace and return
		return str_replace($find, $replace, $msg);
	}

	/**
	 * Generate the error message
	 *
	 * @return  string
	 */
	public function __toString()
	{
		return $this->get_message();
	}
}


