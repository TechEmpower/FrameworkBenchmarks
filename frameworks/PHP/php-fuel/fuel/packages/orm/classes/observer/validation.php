<?php
/**
 * Fuel is a fast, lightweight, community driven PHP5 framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Orm;

/**
 *  Exception to throw when validation failed
 */
class ValidationFailed extends \FuelException
{
	/**
	 * @var  Fieldset the fieldset causing this exception
	 */
	protected $fieldset;

	/**
	 * Overridden \FuelException construct to add a Fieldset instance into the exception
	 *
	 * @param  string  the error message
	 * @param  int  the error code
	 * @param  \Exception any previous exception
	 * @param  \Fieldset  the fieldset on which this exception was triggered
	 */
	public function __construct($message = null, $code = 0, \Exception $previous = null, \Fieldset $fieldset = null)
	{
		parent::__construct($message, $code, $previous);

		$this->fieldset = $fieldset;
	}

	/**
	 * Gets the Fieldset from this exception
	 *
	 * @return Fieldset
	 */
	public function get_fieldset()
	{
		return $this->fieldset;
	}
}

/**
 * Observer class to validate the properties of the model before save.
 *
 * It is also used in Fieldset generation based on a model, to populate the fields
 * and field validation rules of the Fieldset.
 */
class Observer_Validation extends Observer
{

	/**
	 * Set a Model's properties as fields on a Fieldset, which will be created with the Model's
	 * classname if none is provided.
	 *
	 * @param   string
	 * @param   \Fieldset|null
	 * @return  \Fieldset
	 */
	public static function set_fields($obj, $fieldset = null)
	{
		static $_generated = array();
		static $_tabular_rows = array();

		$class = is_object($obj) ? get_class($obj) : $obj;
		if (is_null($fieldset))
		{
			$fieldset = \Fieldset::instance($class);
			if ( ! $fieldset)
			{
				$fieldset = \Fieldset::forge($class);
			}
		}

		// is our parent fieldset a tabular form set?
		$tabular_form = is_object($fieldset->parent()) ? $fieldset->parent()->get_tabular_form() : false;

		// don't cache tabular form fieldsets
		if ( ! $tabular_form)
		{
			! array_key_exists($class, $_generated) and $_generated[$class] = array();
			if (in_array($fieldset, $_generated[$class], true))
			{
				return $fieldset;
			}
			$_generated[$class][] = $fieldset;
		}

		$primary_keys = is_object($obj) ? $obj->primary_key() : $class::primary_key();
		$primary_key = count($primary_keys) === 1 ? reset($primary_keys) : false;
		$properties = is_object($obj) ? $obj->properties() : $class::properties();

		if ($tabular_form and $primary_key and ! is_object($obj))
		{
			isset($_tabular_rows[$class]) or $_tabular_rows[$class] = 0;
		}

		foreach ($properties as $p => $settings)
		{
			if (\Arr::get($settings, 'skip', in_array($p, $primary_keys)))
			{
				continue;
			}

			if (isset($settings['form']['options']))
			{
				foreach ($settings['form']['options'] as $key => $value)
				{
					is_array($value) or $settings['form']['options'][$key] = \Lang::get($value, array(), false) ?: $value;
				}
			}

			// field attributes can be passed in form key
			$attributes = isset($settings['form']) ? $settings['form'] : array();
			// label is either set in property setting, as part of form attributes or defaults to fieldname
			$label = isset($settings['label']) ? $settings['label'] : (isset($attributes['label']) ? $attributes['label'] : $p);
			$label = \Lang::get($label, array(), false) ?: $label;

			// change the fieldname and label for tabular form fieldset children
			if ($tabular_form and $primary_key)
			{
				if (is_object($obj))
				{
					$p = $tabular_form.'['.$obj->{$primary_key}.']['.$p.']';
				}
				else
				{
					$p = $tabular_form.'_new['.$_tabular_rows[$class].']['.$p.']';
				}
				$label = '';
			}

			// create the field and add validation rules
			$field = $fieldset->add($p, $label, $attributes);
			if ( ! empty($settings['validation']))
			{
				foreach ($settings['validation'] as $rule => $args)
				{
					if (is_int($rule) and is_string($args))
					{
						$args = array($args);
					}
					else
					{
						array_unshift($args, $rule);
					}

					call_user_func_array(array($field, 'add_rule'), $args);
				}
			}
		}

		// increase the row counter for tabular row fieldsets
		if ($tabular_form and $primary_key and ! is_object($obj))
		{
			$_tabular_rows[$class]++;
		}

		return $fieldset;
	}

	/**
	 * Execute before saving the Model
	 *
	 * @param   Model	the model object to validate
	 *
	 * @throws  ValidationFailed
	 */
	public function before_save(Model $obj)
	{
		$this->validate($obj);
	}

	/**
	 * Validate the model
	 *
	 * @param   Model	the model object to validate
	 *
	 * @throws  ValidationFailed
	 */
	public function validate(Model $obj)
	{
		$fieldset = static::set_fields($obj);
		$val = $fieldset->validation();

		// only allow partial validation on updates, specify the fields for updates to allow null
		$allow_partial = $obj->is_new() ? false : array();

		$input = array();
		foreach (array_keys($obj->properties()) as $p)
		{
			if ( ! in_array($p, $obj->primary_key()) and $obj->is_changed($p))
			{
				$input[$p] = $obj->{$p};
				is_array($allow_partial) and $allow_partial[] = $p;
			}
		}

		if ( ! empty($input) and $val->run($input, $allow_partial, array($obj)) === false)
		{
			throw new ValidationFailed($val->show_errors(), 0, null, $fieldset);
		}
		else
		{
			foreach ($input as $k => $v)
			{
				$obj->{$k} = $val->validated($k);
			}
		}
	}
}
