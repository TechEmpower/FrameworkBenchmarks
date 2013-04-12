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



// ------------------------------------------------------------------------

/**
 * Validation
 *
 * Static object to allow static usage of validation through singleton.
 *
 * @package     Fuel
 * @subpackage  Core
 * @category    Core
 */
class Validation
{
	/**
	 * @var  Validation  keeps a reference to an instance of Validation while it is being run
	 */
	protected static $active;

	/**
	 * @var  Fieldset_Field  keeps a reference to an instance of the Fieldset_Field validation is being run on
	 */
	protected static $active_field;

	/**
	* Gets a new instance of the Validation class.
	*
	* @param   string      The name or instance of the Fieldset to link to
	* @return  Validation
	*/
	public static function forge($fieldset = 'default')
	{
		if (is_string($fieldset))
		{
			($set = \Fieldset::instance($fieldset)) and $fieldset = $set;
		}

		if ($fieldset instanceof Fieldset)
		{
			if ($fieldset->validation(false) != null)
			{
				throw new \DomainException('Form instance already exists, cannot be recreated. Use instance() instead of forge() to retrieve the existing instance.');
			}
		}

		return new static($fieldset);
	}

	public static function instance($name = null)
	{
		$fieldset = \Fieldset::instance($name);
		return $fieldset === false ? false : $fieldset->validation();
	}

	/**
	 * Fetch the currently active validation instance
	 *
	 * @return  Validation
	 */
	public static function active()
	{
		return static::$active;
	}

	/**
	 * Set or unset the currently active validation instance
	 */
	protected static function set_active($instance = null)
	{
		static::$active = $instance;
	}

	/**
	 * Fetch the field currently being validated
	 */
	public static function active_field()
	{
		return static::$active_field;
	}

	/**
	 * Set or unset the current field being validated
	 */
	protected static function set_active_field($instance = null)
	{
		static::$active_field = $instance;
	}

	/**
	 * @var  Fieldset  the fieldset this instance validates
	 */
	protected $fieldset;

	/**
	 * @var  array  available after validation started running: contains given input values
	 */
	protected $input = array();

	/**
	 * @var  array  contains values of fields that validated successfully
	 */
	protected $validated = array();

	/**
	 * @var  array  contains Validation_Error instances of encountered errors
	 */
	protected $errors = array();

	/**
	 * @var  array  contains a list of classnames and objects that may contain validation methods
	 */
	protected $callables = array();

	/**
	 * @var  bool  $global_input_fallback  wether to fall back to Input::param
	 */
	protected $global_input_fallback = true;

	/**
	 * @var  array  contains validation error messages, will overwrite those from lang files
	 */
	protected $error_messages = array();

	protected function __construct($fieldset)
	{
		if ($fieldset instanceof Fieldset)
		{
			$fieldset->validation($this);
			$this->fieldset = $fieldset;
		}
		else
		{
			$this->fieldset = \Fieldset::forge($fieldset, array('validation_instance' => $this));
		}

		$this->callables = array($this);
		$this->global_input_fallback = \Config::get('validation.global_input_fallback', true);
	}

	/**
	 * Returns the related fieldset
	 *
	 * @return  Fieldset
	 */
	public function fieldset()
	{
		return $this->fieldset;
	}

	/**
	 * Simpler alias for Validation->add()
	 *
	 * @param   string  Field name
	 * @param   string  Field label
	 * @param   string  Rules as a piped string
	 * @return  Fieldset_Field  $this to allow chaining
	 * @depricated  Remove in v2.0, passing rules as string is to be removed use add() instead
	 */
	public function add_field($name, $label, $rules)
	{
		$field = $this->add($name, $label);

		is_array($rules) or $rules = explode('|', $rules);

		foreach ($rules as $rule)
		{
			if (($pos = strpos($rule, '[')) !== false)
			{
				preg_match('#\[(.*)\]#', $rule, $param);
				$rule = substr($rule, 0, $pos);

				// deal with rules that have comma's in the rule parameter
				if (in_array($rule, array('match_pattern')))
				{
					call_user_func_array(array($field, 'add_rule'), array_merge(array($rule), array($param[1])));
				}
				elseif (in_array($rule, array('valid_string')))
				{
					call_user_func_array(array($field, 'add_rule'), array_merge(array($rule), array(explode(',', $param[1]))));
				}
				else
				{
					call_user_func_array(array($field, 'add_rule'), array_merge(array($rule), explode(',', $param[1])));
				}
			}
			else
			{
				$field->add_rule($rule);
			}
		}

		return $field;

	}

	/**
	 * This will overwrite lang file messages for this validation instance
	 *
	 * @param   string
	 * @param   string
	 * @return  Validation  this, to allow chaining
	 */
	public function set_message($rule, $message)
	{
		if ($message !== null)
		{
			$this->error_messages[$rule] = $message;
		}
		else
		{
			unset($this->error_messages[$rule]);
		}

		return $this;
	}

	/**
	 * Fetches a specific error message for this validation instance
	 *
	 * @param   string
	 * @return  string
	 */
	public function get_message($rule)
	{
		if ( ! array_key_exists($rule, $this->error_messages))
		{
			return false;
		}

		return $this->error_messages[$rule];
	}

	/**
	 * Add Callable
	 *
	 * Adds an object for which you don't need to write a full callback, just
	 * the method as a string will do. This also allows for overwriting functionality
	 * from this object because the new class is prepended.
	 *
	 * @param   string|Object  Classname or object
	 * @return  Validation     this, to allow chaining
	 */
	public function add_callable($class)
	{
		if ( ! (is_object($class) || class_exists($class)))
		{
			throw new \InvalidArgumentException('Input for add_callable is not a valid object or class.');
		}

		// Prevent having the same class twice in the array, remove to re-add on top if...
		foreach ($this->callables as $key => $c)
		{
			// ...it already exists in callables
			if ($c === $class)
			{
				unset($this->callables[$key]);
			}
			// ...new object/class extends it or an instance of it
			elseif (is_string($c) and (is_subclass_of($class, $c) or (is_object($class) and is_a($class, $c))))
			{
				unset($this->callables[$key]);
			}
			// but if there's a subclass in there to the new one, put the subclass on top and forget the new
			elseif (is_string($class) and (is_subclass_of($c, $class) or (is_object($c) and is_a($c, $class))))
			{
				unset($this->callables[$key]);
				$class = $c;
			}
		}

		array_unshift($this->callables, $class);

		return $this;
	}

	/*
	 * Remove Callable
	 *
	 * Removes an object from the callables array
	 *
	 * @param   string|Object  Classname or object
	 * @return  Validation     this, to allow chaining
	 */
	public function remove_callable($class)
	{
		if (($key = array_search($class, $this->callables, true)))
		{
			unset($this->callables[$key]);
		}

		return $this;
	}

	/**
	 * Fetch the objects for which you don't need to add a full callback but
	 * just the method name
	 *
	 * @return  array
	 */
	public function callables()
	{
		return $this->callables;
	}

	/**
	 * Run validation
	 *
	 * Performs validation with current fieldset and on given input, will try POST
	 * when input wasn't given.
	 *
	 * @param   array  input that overwrites POST values
	 * @param   bool   will skip validation of values it can't find or are null
	 * @return  bool   whether validation succeeded
	 */
	public function run($input = null, $allow_partial = false, $temp_callables = array())
	{
		if (is_null($input) and \Input::method() != 'POST')
		{
			return false;
		}

		// Backup current state of callables so they can be restored after adding temp callables
		$callable_backup = $this->callables;

		// Add temporary callables, reversed so first ends on top
		foreach (array_reverse($temp_callables) as $temp_callable)
		{
			$this->add_callable($temp_callable);
		}

		static::set_active($this);

		$this->validated = array();
		$this->errors = array();
		$this->input = $input ?: array();
		$fields = $this->field(null, true);
		foreach($fields as $field)
		{
			static::set_active_field($field);

			// convert form field array's to Fuel dotted notation
			$name = str_replace(array('[',']'), array('.', ''), $field->name);

			$value = $this->input($name);
			if (($allow_partial === true and $value === null)
				or (is_array($allow_partial) and ! in_array($field->name, $allow_partial)))
			{
				continue;
			}
			try
			{
				foreach ($field->rules as $rule)
				{
					$callback  = $rule[0];
					$params    = $rule[1];
					$this->_run_rule($callback, $value, $params, $field);
				}
				if (strpos($name, '.') !== false)
				{
					\Arr::set($this->validated, $name, $value);
				}
				else
				{
					$this->validated[$name] = $value;
				}
			}
			catch (Validation_Error $v)
			{
				$this->errors[$field->name] = $v;

				if($field->fieldset())
				{
					$field->fieldset()->Validation()->add_error($field->name, $v);
				}
			}
		}

		static::set_active();
		static::set_active_field();

		// Restore callables
		$this->callables = $callable_backup;

		return empty($this->errors);
	}

	/**
	 * Takes the rule input and formats it into a name & callback
	 *
	 * @param   string|array  short rule to be called on Validation callables array or full callback
	 * @return  array|bool    rule array or false when it fails to find something callable
	 */
	protected function _find_rule($callback)
	{
		// Rules are validated and only accepted when given as an array consisting of
		// array(callback, params) or just callbacks in an array.
		if (is_string($callback))
		{
			$callback_method = '_validation_'.$callback;
			foreach ($this->callables as $callback_class)
			{
				if (method_exists($callback_class, $callback_method))
				{
					return array($callback => array($callback_class, $callback_method));
				}
			}
		}

		// when no callable function was found, try regular callbacks
		if (is_callable($callback))
		{
			if ($callback instanceof \Closure)
			{
				$callback_name = 'closure';
			}
			elseif (is_array($callback))
			{
				$callback_name = preg_replace('#^([a-z_]*\\\\)*#i', '',
					is_object($callback[0]) ? get_class($callback[0]) : $callback[0]).':'.$callback[1];
			}
			else
			{
				$callback_name = preg_replace('#^([a-z_]*\\\\)*#i', '', str_replace('::', ':', $callback));
			}
			return array($callback_name => $callback);
		}
		elseif (is_array($callback) and is_callable(reset($callback)))
		{
			return $callback;
		}
		else
		{
			$string = ! is_array($callback)
					? $callback
					: (is_object(@$callback[0])
						? get_class(@$callback[0]).'->'.@$callback[1]
						: @$callback[0].'::'.@$callback[1]);
			\Error::notice('Invalid rule "'.$string.'" passed to Validation, not used.');
			return false;
		}
	}

	/**
	 * Run rule
	 *
	 * Performs a single rule on a field and its value
	 *
	 * @param   callback
	 * @param   mixed     Value by reference, will be edited
	 * @param   array     Extra parameters
	 * @param   array     Validation field description
	 * @throws  Validation_Error
	 */
	protected function _run_rule($rule, &$value, $params, $field)
	{
		if (($rule = $this->_find_rule($rule)) === false)
		{
			return;
		}

		$output = call_user_func_array(reset($rule), array_merge(array($value), $params));

		if ($output === false && $value !== false)
		{
			throw new \Validation_Error($field, $value, $rule, $params);
		}
		elseif ($output !== true)
		{
			$value = $output;
		}
	}

	/**
	 * Fetches the input value from either post or given input
	 *
	 * @param   string
	 * @param   mixed
	 * @return  mixed|array  the input value or full input values array
	 */
	public function input($key = null, $default = null)
	{
		if ($key === null)
		{
			return $this->input;
		}

		if ( ! array_key_exists($key, $this->input))
		{
			if (strpos($key,'[') !== false)
			{
				$this->input[$key] =  $this->global_input_fallback ? \Arr::get(\Input::param(), str_replace(array('[', ']'),array('.', ''),$key), $default) : $default;
			}
			else
			{
				$this->input[$key] =  $this->global_input_fallback ? \Input::param($key, $default) : $default;
			}
		}

		return $this->input[$key];
	}

	/**
	 * Validated
	 *
	 * Returns specific validated value or all validated field=>value pairs
	 *
	 * @param   string  fieldname
	 * @param   mixed   value to return when not validated
	 * @return  mixed|array  the validated value or full validated values array
	 */
	public function validated($field = null, $default = false)
	{
		if ($field === null)
		{
			return $this->validated;
		}

		return array_key_exists($field, $this->validated) ? $this->validated[$field] : $default;
	}

	/**
	 * Error
	 *
	 * Return specific error or all errors thrown during validation
	 *
	 * @param   string  fieldname
	 * @param   mixed   value to return when not validated
	 * @return  Validation_Error|array  the validation error object or full array of error objects
	 */
	public function error($field = null, $default = false)
	{
		if ($field === null)
		{
			return $this->errors;
		}

		return array_key_exists($field, $this->errors) ? $this->errors[$field] : $default;
	}

	/**
	 * Show errors
	 *
	 * Returns all errors in a list or with set markup from $options param
	 *
	 * @param   array  uses keys open_list, close_list, open_error, close_error & no_errors
	 * @return  string
	 */
	public function show_errors($options = array())
	{
		$default = array(
			'open_list'    => \Config::get('validation.open_list', '<ul>'),
			'close_list'   => \Config::get('validation.close_list', '</ul>'),
			'open_error'   => \Config::get('validation.open_error', '<li>'),
			'close_error'  => \Config::get('validation.close_error', '</li>'),
			'no_errors'    => \Config::get('validation.no_errors', '')
		);
		$options = array_merge($default, $options);

		if (empty($this->errors))
		{
			return $options['no_errors'];
		}

		$output = $options['open_list'];
		foreach($this->errors as $e)
		{
			$output .= $options['open_error'].$e->get_message().$options['close_error'];
		}
		$output .= $options['close_list'];

		return $output;
	}

	/**
	 * Add error
	 *
	 * Adds an error for a given field.
	 *
	 * @param   string				field name for which to set the error
	 * @param   Validation_Error  	error for the field
	 * @return  Validation 			this, to allow chaining
	 */
	protected function add_error($name = null, $error = null)
	{
		if($name !== null and $error !== null)
		{
			$this->errors[$name] = $error;
		}

		return $this;
	}

	/**
	 * Alias for $this->fieldset->add()
	 *
	 * @return  Fieldset_Field
	 */
	public function add($name, $label = '', array $attributes = array(), array $rules = array())
	{
		return $this->fieldset->add($name, $label, $attributes, $rules);
	}

	/**
	 * Alias for $this->fieldset->add_model()
	 *
	 * @return  Validation  this, to allow chaining
	 */
	public function add_model($class, $instance = null, $method = 'set_form_fields')
	{
		$this->fieldset->add_model($class, $instance, $method);

		return $this;
	}

	/**
	 * Alias for $this->fieldset->field()
	 *
	 * @return  Fieldset_Field
	 */
	public function field($name = null, $flatten = false)
	{
		return $this->fieldset->field($name, $flatten);
	}

	/* -------------------------------------------------------------------------------
	 * The validation methods
	 * ------------------------------------------------------------------------------- */

	/**
	 * Required
	 *
	 * Value may not be empty
	 *
	 * @param   mixed
	 * @return  bool
	 */
	public function _validation_required($val)
	{
		return ! $this->_empty($val);
	}

	/**
	 * Special empty method because 0 and '0' are non-empty values
	 *
	 * @param   mixed
	 * @return  bool
	 */
	public static function _empty($val)
	{
		return ($val === false or $val === null or $val === '' or $val === array());
	}

	/**
	 * Match value against comparison input
	 *
	 * @param   mixed
	 * @param   mixed
	 * @param   bool  whether to do type comparison
	 * @return  bool
	 */
	public function _validation_match_value($val, $compare, $strict = false)
	{
		// first try direct match
		if ($this->_empty($val) || $val === $compare || ( ! $strict && $val == $compare))
		{
			return true;
		}

		// allow multiple input for comparison
		if (is_array($compare))
		{
			foreach($compare as $c)
			{
				if ($val === $c || ( ! $strict && $val == $c))
				{
					return true;
				}
			}
		}

		// all is lost, return failure
		return false;
	}

	/**
	 * Match PRCE pattern
	 *
	 * @param   string
	 * @param   string  a PRCE regex pattern
	 * @return  bool
	 */
	public function _validation_match_pattern($val, $pattern)
	{
		return $this->_empty($val) || preg_match($pattern, $val) > 0;
	}

	/**
	 * Match specific other submitted field string value
	 * (must be both strings, check is type sensitive)
	 *
	 * @param   string
	 * @param   string
	 * @return  bool
	 */
	public function _validation_match_field($val, $field)
	{
		if ($this->input($field) !== $val)
		{
			$validating = $this->active_field();
			throw new \Validation_Error($validating, $val, array('match_field' => array($field)), array($this->field($field)->label));
		}

		return true;
	}

	/**
	 * Minimum string length
	 *
	 * @param   string
	 * @param   int
	 * @return  bool
	 */
	public function _validation_min_length($val, $length)
	{
		return $this->_empty($val) || (MBSTRING ? mb_strlen($val) : strlen($val)) >= $length;
	}

	/**
	 * Maximum string length
	 *
	 * @param   string
	 * @param   int
	 * @return  bool
	 */
	public function _validation_max_length($val, $length)
	{
		return $this->_empty($val) || (MBSTRING ? mb_strlen($val) : strlen($val)) <= $length;
	}

	/**
	 * Exact string length
	 *
	 * @param   string
	 * @param   int
	 * @return  bool
	 */
	public function _validation_exact_length($val, $length)
	{
		return $this->_empty($val) || (MBSTRING ? mb_strlen($val) : strlen($val)) == $length;
	}

	/**
	 * Validate email using PHP's filter_var()
	 *
	 * @param   string
	 * @return  bool
	 */
	public function _validation_valid_email($val)
	{
		return $this->_empty($val) || filter_var($val, FILTER_VALIDATE_EMAIL);
	}

	/**
	 * Validate email using PHP's filter_var()
	 *
	 * @param   string
	 * @return  bool
	 */
	public function _validation_valid_emails($val, $separator = ',')
	{
		if ($this->_empty($val))
		{
			return true;
		}

		$emails = explode($separator, $val);

		foreach ($emails as $e)
		{
			if ( ! filter_var(trim($e), FILTER_VALIDATE_EMAIL))
			{
				return false;
			}
		}
		return true;
	}

	/**
	 * Validate URL using PHP's filter_var()
	 *
	 * @param   string
	 * @return  bool
	 */
	public function _validation_valid_url($val)
	{
		return $this->_empty($val) || filter_var($val, FILTER_VALIDATE_URL);
	}

	/**
	 * Validate IP using PHP's filter_var()
	 *
	 * @param   string
	 * @return  bool
	 */
	public function _validation_valid_ip($val)
	{
		return $this->_empty($val) || filter_var($val, FILTER_VALIDATE_IP);
	}

	/**
	 * Validate input string with many options
	 *
	 * @param   string
	 * @param   string|array  either a named filter or combination of flags
	 * @return  bool
	 */
	public function _validation_valid_string($val, $flags = array('alpha', 'utf8'))
	{
		if ($this->_empty($val))
		{
			return true;
		}

		if ( ! is_array($flags))
		{
			if ($flags == 'alpha')
			{
				$flags = array('alpha', 'utf8');
			}
			elseif ($flags == 'alpha_numeric')
			{
				$flags = array('alpha', 'utf8', 'numeric');
			}
			elseif ($flags == 'url_safe')
			{
				$flags = array('alpha', 'numeric', 'dashes');
			}
			elseif ($flags == 'integer' or $flags == 'numeric')
			{
				$flags = array('numeric');
			}
			elseif ($flags == 'float')
			{
				$flags = array('numeric', 'dots');
			}
			elseif ($flags == 'quotes')
			{
				$flags = array('singlequotes', 'doublequotes');
			}
			elseif ($flags == 'all')
			{
				$flags = array('alpha', 'utf8', 'numeric', 'spaces', 'newlines', 'tabs', 'punctuation', 'singlequotes', 'doublequotes', 'dashes');
			}
			else
			{
				return false;
			}
		}

		$pattern = ! in_array('uppercase', $flags) && in_array('alpha', $flags) ? 'a-z' : '';
		$pattern .= ! in_array('lowercase', $flags) && in_array('alpha', $flags) ? 'A-Z' : '';
		$pattern .= in_array('numeric', $flags) ? '0-9' : '';
		$pattern .= in_array('spaces', $flags) ? ' ' : '';
		$pattern .= in_array('newlines', $flags) ? "\n" : '';
		$pattern .= in_array('tabs', $flags) ? "\t" : '';
		$pattern .= in_array('dots', $flags) && ! in_array('punctuation', $flags) ? '\.' : '';
		$pattern .= in_array('commas', $flags) && ! in_array('punctuation', $flags) ? ',' : '';
		$pattern .= in_array('punctuation', $flags) ? "\.,\!\?:;\&" : '';
		$pattern .= in_array('dashes', $flags) ? '_\-' : '';
		$pattern .= in_array('singlequotes', $flags) ? "'" : '';
		$pattern .= in_array('doublequotes', $flags) ? "\"" : '';
		$pattern = empty($pattern) ? '/^(.*)$/' : ('/^(['.$pattern.'])+$/');
		$pattern .= in_array('utf8', $flags) ? 'u' : '';

		return preg_match($pattern, $val) > 0;
	}

	/**
	 * Checks whether numeric input has a minimum value
	 *
	 * @param   string|float|int
	 * @param   float|int
	 * @return  bool
	 */
	public function _validation_numeric_min($val, $min_val)
	{
		return $this->_empty($val) || floatval($val) >= floatval($min_val);
	}

	/**
	 * Checks whether numeric input has a maximum value
	 *
	 * @param   string|float|int
	 * @param   float|int
	 * @return  bool
	 */
	public function _validation_numeric_max($val, $max_val)
	{
		return $this->_empty($val) || floatval($val) <= floatval($max_val);
	}

	/**
	 * Checks whether numeric input is between a minimum and a maximum value
	 *
	 * @param   string|float|int
	 * @param   float|int
	 * @param   float|int
	 * @return  bool
	 */
	public function _validation_numeric_between($val, $min_val, $max_val)
	{
		return $this->_empty($val) or (floatval($val) >= floatval($min_val) and floatval($val) <= floatval($max_val));
	}

	/**
	 * Conditionally requires completion of current field based on completion of another field
	 *
	 * @param mixed
	 * @param string
	 * @return bool
	 */
	public function _validation_required_with($val, $field)
	{
		if ( ! $this->_empty($this->input($field)) and $this->_empty($val))
		{
			$validating = $this->active_field();
			throw new \Validation_Error($validating, $val, array('required_with' => array($this->field($field))), array($this->field($field)->label));
		}

		return true;
	}
}
