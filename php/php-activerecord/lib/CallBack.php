<?php
/**
 * @package ActiveRecord
 */
namespace ActiveRecord;
use Closure;

/**
 * Callbacks allow the programmer to hook into the life cycle of a {@link Model}.
 *
 * You can control the state of your object by declaring certain methods to be
 * called before or after methods are invoked on your object inside of ActiveRecord.
 *
 * Valid callbacks are:
 * <ul>
 * <li><b>after_construct:</b> called after a model has been constructed</li>
 * <li><b>before_save:</b> called before a model is saved</li>
 * <li><b>after_save:</b> called after a model is saved</li>
 * <li><b>before_create:</b> called before a NEW model is to be inserted into the database</li>
 * <li><b>after_create:</b> called after a NEW model has been inserted into the database</li>
 * <li><b>before_update:</b> called before an existing model has been saved</li>
 * <li><b>after_update:</b> called after an existing model has been saved</li>
 * <li><b>before_validation:</b> called before running validators</li>
 * <li><b>after_validation:</b> called after running validators</li>
 * <li><b>before_validation_on_create:</b> called before validation on a NEW model being inserted</li>
 * <li><b>after_validation_on_create:</b> called after validation on a NEW model being inserted</li>
 * <li><b>before_validation_on_update:</b> see above except for an existing model being saved</li>
 * <li><b>after_validation_on_update:</b> ...</li>
 * <li><b>before_destroy:</b> called after a model has been deleted</li>
 * <li><b>after_destroy:</b> called after a model has been deleted</li>
 * </ul>
 *
 * This class isn't meant to be used directly. Callbacks are defined on your model like the example below:
 *
 * <code>
 * class Person extends ActiveRecord\Model {
 *   static $before_save = array('make_name_uppercase');
 *   static $after_save = array('do_happy_dance');
 *
 *   public function make_name_uppercase() {
 *     $this->name = strtoupper($this->name);
 *   }
 *
 *   public function do_happy_dance() {
 *     happy_dance();
 *   }
 * }
 * </code>
 *
 * Available options for callbacks:
 *
 * <ul>
 * <li><b>prepend:</b> puts the callback at the top of the callback chain instead of the bottom</li>
 * </ul>
 *
 * @package ActiveRecord
 * @link http://www.phpactiverecord.org/guides/callbacks
 */
class CallBack
{
	/**
	 * List of available callbacks.
	 *
	 * @var array
	 */
	static protected $VALID_CALLBACKS = array(
		'after_construct',
		'before_save',
		'after_save',
		'before_create',
		'after_create',
		'before_update',
		'after_update',
		'before_validation',
		'after_validation',
		'before_validation_on_create',
		'after_validation_on_create',
		'before_validation_on_update',
		'after_validation_on_update',
		'before_destroy',
		'after_destroy'
	);

	/**
	 * Container for reflection class of given model
	 *
	 * @var object
	 */
	private $klass;

	/**
	 * List of public methods of the given model
	 * @var array
	 */
	private $publicMethods;

	/**
	 * Holds data for registered callbacks.
	 *
	 * @var array
	 */
	private $registry = array();

	/**
	 * Creates a CallBack.
	 *
	 * @param string $model_class_name The name of a {@link Model} class
	 * @return CallBack
	 */
	public function __construct($model_class_name)
	{
		$this->klass = Reflections::instance()->get($model_class_name);

		foreach (static::$VALID_CALLBACKS as $name)
		{
			// look for explicitly defined static callback
			if (($definition = $this->klass->getStaticPropertyValue($name,null)))
			{
				if (!is_array($definition))
					$definition = array($definition);

				foreach ($definition as $method_name)
					$this->register($name,$method_name);
			}

			// implicit callbacks that don't need to have a static definition
			// simply define a method named the same as something in $VALID_CALLBACKS
			// and the callback is auto-registered
			elseif ($this->klass->hasMethod($name))
				$this->register($name,$name);
		}
	}

	/**
	 * Returns all the callbacks registered for a callback type.
	 *
	 * @param $name string Name of a callback (see {@link VALID_CALLBACKS $VALID_CALLBACKS})
	 * @return array array of callbacks or null if invalid callback name.
	 */
	public function get_callbacks($name)
	{
		return isset($this->registry[$name]) ? $this->registry[$name] : null;
	}

	/**
	 * Invokes a callback.
	 *
	 * @internal This is the only piece of the CallBack class that carries its own logic for the
	 * model object. For (after|before)_(create|update) callbacks, it will merge with
	 * a generic 'save' callback which is called first for the lease amount of precision.
	 *
	 * @param string $model Model to invoke the callback on.
	 * @param string $name Name of the callback to invoke
	 * @param boolean $must_exist Set to true to raise an exception if the callback does not exist.
	 * @return mixed null if $name was not a valid callback type or false if a method was invoked
	 * that was for a before_* callback and that method returned false. If this happens, execution
	 * of any other callbacks after the offending callback will not occur.
	 */
	public function invoke($model, $name, $must_exist=true)
	{
		if ($must_exist && !array_key_exists($name, $this->registry))
			throw new ActiveRecordException("No callbacks were defined for: $name on " . get_class($model));

		// if it doesn't exist it might be a /(after|before)_(create|update)/ so we still need to run the save
		// callback
		if (!array_key_exists($name, $this->registry))
			$registry = array();
		else
			$registry = $this->registry[$name];

		$first = substr($name,0,6);

		// starts with /(after|before)_(create|update)/
		if (($first == 'after_' || $first == 'before') && (($second = substr($name,7,5)) == 'creat' || $second == 'updat' || $second == 'reate' || $second == 'pdate'))
		{
			$temporal_save = str_replace(array('create', 'update'), 'save', $name);

			if (!isset($this->registry[$temporal_save]))
				$this->registry[$temporal_save] = array();

			$registry = array_merge($this->registry[$temporal_save], $registry ? $registry : array());
		}

		if ($registry)
		{
			foreach ($registry as $method)
			{
				$ret = ($method instanceof Closure ? $method($model) : $model->$method());

				if (false === $ret && $first === 'before')
					return false;
			}
		}
		return true;
	}

	/**
	 * Register a new callback.
	 *
	 * The option array can contain the following parameters:
	 * <ul>
	 * <li><b>prepend:</b> Add this callback at the beginning of the existing callbacks (true) or at the end (false, default)</li>
	 * </ul>
	 *
	 * @param string $name Name of callback type (see {@link VALID_CALLBACKS $VALID_CALLBACKS})
	 * @param mixed $closure_or_method_name Either a closure or the name of a method on the {@link Model}
	 * @param array $options Options array
	 * @return void
	 * @throws ActiveRecordException if invalid callback type or callback method was not found
	 */
	public function register($name, $closure_or_method_name=null, $options=array())
	{
		$options = array_merge(array('prepend' => false), $options);

		if (!$closure_or_method_name)
			$closure_or_method_name = $name;

		if (!in_array($name,self::$VALID_CALLBACKS))
			throw new ActiveRecordException("Invalid callback: $name");

		if (!($closure_or_method_name instanceof Closure))
		{
			if (!isset($this->publicMethods))
				$this->publicMethods = get_class_methods($this->klass->getName());

			if (!in_array($closure_or_method_name, $this->publicMethods))
			{
				if ($this->klass->hasMethod($closure_or_method_name))
				{
					// Method is private or protected
					throw new ActiveRecordException("Callback methods need to be public (or anonymous closures). " .
						"Please change the visibility of " . $this->klass->getName() . "->" . $closure_or_method_name . "()");
				}
				else
				{
					// i'm a dirty ruby programmer
					throw new ActiveRecordException("Unknown method for callback: $name" .
						(is_string($closure_or_method_name) ? ": #$closure_or_method_name" : ""));
				}
			}
		}

		if (!isset($this->registry[$name]))
			$this->registry[$name] = array();

		if ($options['prepend'])
			array_unshift($this->registry[$name], $closure_or_method_name);
		else
			$this->registry[$name][] = $closure_or_method_name;
	}
}
?>
