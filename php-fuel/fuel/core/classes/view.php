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
 * View class
 *
 * Acts as an object wrapper for HTML pages with embedded PHP, called "views".
 * Variables can be assigned with the view object and referenced locally within
 * the view.
 *
 * @package   Fuel
 * @category  Core
 * @link      http://docs.fuelphp.com/classes/view.html
 */
class View
{

	/**
	 * @var  array  Global view data
	 */
	protected static $global_data = array();

	/**
	 * @var  array  Holds a list of specific filter rules for global variables
	 */
	protected static $global_filter = array();

	/**
	 * @var  array  Current active search paths
	 */
	protected $request_paths = array();

	/**
	 * @var  bool  Whether to auto-filter the view's data
	 */
	protected $auto_filter = true;

	/**
	 * @var  array  Holds a list of specific filter rules for local variables
	 */
	protected $local_filter = array();

	/**
	 * @var  string  The view's filename
	 */
	protected $file_name = null;

	/**
	 * @var  array  The view's data
	 */
	protected $data = array();

	/**
	 * @var  string  The view file extension
	 */
	protected $extension = 'php';

	/**
	 * @var  Request  active request when the View was created
	 */
	protected $active_request = null;

	/**
	 * @var  string  active language at the time the object was created
	 */
	protected $active_language = null;

	/**
	 * Returns a new View object. If you do not define the "file" parameter,
	 * you must call [static::set_filename].
	 *
	 *     $view = View::forge($file);
	 *
	 * @param   string  view filename
	 * @param   array   array of values
	 * @return  View
	 */
	public static function forge($file = null, $data = null, $auto_filter = null)
	{
		return new static($file, $data, $auto_filter);
	}

	/**
	 * Sets the initial view filename and local data.
	 *
	 *     $view = new View($file);
	 *
	 * @param   string  view filename
	 * @param   array   array of values
	 * @return  void
	 * @uses    View::set_filename
	 */
	public function __construct($file = null, $data = null, $filter = null)
	{
		if (is_object($data) === true)
		{
			$data = get_object_vars($data);
		}
		elseif ($data and ! is_array($data))
		{
			throw new \InvalidArgumentException('The data parameter only accepts objects and arrays.');
		}

		$this->auto_filter = is_null($filter) ? \Config::get('security.auto_filter_output', true) : $filter;

		if ($file !== null)
		{
			$this->set_filename($file);
		}

		if ($data !== null)
		{
			// Add the values to the current data
			$this->data = $data;
		}

		// store the current request search paths to deal with out-of-context rendering
		if (class_exists('Request', false) and $active = \Request::active() and \Request::main() != $active)
		{
			$this->request_paths = $active->get_paths();
		}
		isset($active) and $this->active_request = $active;

		// store the active language, so we can render the view in the correct language later
		$this->active_language = \Config::get('language', 'en');
	}

	/**
	 * Magic method, searches for the given variable and returns its value.
	 * Local variables will be returned before global variables.
	 *
	 *     $value = $view->foo;
	 *
	 * @param   string  variable name
	 * @return  mixed
	 * @throws  OutOfBoundsException
	 */
	public function & __get($key)
	{
		return $this->get($key);
	}

	/**
	 * Magic method, calls [static::set] with the same parameters.
	 *
	 *     $view->foo = 'something';
	 *
	 * @param   string  variable name
	 * @param   mixed   value
	 * @return  void
	 */
	public function __set($key, $value)
	{
		$this->set($key, $value);
	}

	/**
	 * Magic method, determines if a variable is set.
	 *
	 *     isset($view->foo);
	 *
	 * [!!] `null` variables are not considered to be set by [isset](http://php.net/isset).
	 *
	 * @param   string  variable name
	 * @return  boolean
	 */
	public function __isset($key)
	{
		return (isset($this->data[$key]) or isset(static::$global_data[$key]));
	}

	/**
	 * Magic method, unsets a given variable.
	 *
	 *     unset($view->foo);
	 *
	 * @param   string  variable name
	 * @return  void
	 */
	public function __unset($key)
	{
		unset($this->data[$key], static::$global_data[$key]);
	}

	/**
	 * Magic method, returns the output of [static::render].
	 *
	 * @return  string
	 * @uses    View::render
	 */
	public function __toString()
	{
		try
		{
			return $this->render();
		}
		catch (\Exception $e)
		{
			\Error::exception_handler($e);

			return '';
		}
	}

	/**
	 * Captures the output that is generated when a view is included.
	 * The view data will be extracted to make local variables. This method
	 * is static to prevent object scope resolution.
	 *
	 *     $output = $this->process_file();
	 *
	 * @param   string  File override
	 * @param   array   variables
	 * @return  string
	 */
	protected function process_file($file_override = false)
	{
		$clean_room = function($__file_name, array $__data)
		{
			extract($__data, EXTR_REFS);

			// Capture the view output
			ob_start();

			try
			{
				// Load the view within the current scope
				include $__file_name;
			}
			catch (\Exception $e)
			{
				// Delete the output buffer
				ob_end_clean();

				// Re-throw the exception
				throw $e;
			}

			// Get the captured output and close the buffer
			return ob_get_clean();
		};
		return $clean_room($file_override ?: $this->file_name, $this->get_data());
	}

	/**
	 * Retrieves all the data, both local and global.  It filters the data if
	 * necessary.
	 *
	 *     $data = $this->get_data();
	 *
	 * @param   string  $scope  local/glocal/all
	 * @return  array   view data
	 */
	protected function get_data($scope = 'all')
	{
		$clean_it = function ($data, $rules, $auto_filter)
		{
			foreach ($data as $key => &$value)
			{
				$filter = array_key_exists($key, $rules) ? $rules[$key] : null;
				$filter = is_null($filter) ? $auto_filter : $filter;

				$value = $filter ? \Security::clean($value, null, 'security.output_filter') : $value;
			}

			return $data;
		};

		$data = array();

		if ( ! empty($this->data)  and ($scope === 'all' or $scope === 'local'))
		{
			$data += $clean_it($this->data, $this->local_filter, $this->auto_filter);
		}

		if ( ! empty(static::$global_data)  and ($scope === 'all' or $scope === 'global'))
		{
			$data += $clean_it(static::$global_data, static::$global_filter, $this->auto_filter);
		}

		return $data;
	}

	/**
	 * Sets a global variable, similar to [static::set], except that the
	 * variable will be accessible to all views.
	 *
	 *     View::set_global($name, $value);
	 *
	 * @param   string  variable name or an array of variables
	 * @param   mixed   value
	 * @param   bool    whether to filter the data or not
	 * @return  void
	 */
	public static function set_global($key, $value = null, $filter = null)
	{
		if (is_array($key))
		{
			foreach ($key as $name => $value)
			{
				if ($filter !== null)
				{
					static::$global_filter[$name] = $filter;
				}
				static::$global_data[$name] = $value;
			}
		}
		else
		{
			if ($filter !== null)
			{
				static::$global_filter[$key] = $filter;
			}
			static::$global_data[$key] = $value;
		}
	}

	/**
	 * Assigns a global variable by reference, similar to [static::bind], except
	 * that the variable will be accessible to all views.
	 *
	 *     View::bind_global($key, $value);
	 *
	 * @param   string  variable name
	 * @param   mixed   referenced variable
	 * @param   bool    whether to filter the data or not
	 * @return  void
	 */
	public static function bind_global($key, &$value, $filter = null)
	{
		if ($filter !== null)
		{
			static::$global_filter[$key] = $filter;
		}
		static::$global_data[$key] =& $value;
	}

	/**
	 * Sets whether to filter the data or not.
	 *
	 *     $view->auto_filter(false);
	 *
	 * @param   bool  whether to auto filter or not
	 * @return  View
	 */
	public function auto_filter($filter = true)
	{
		if (func_num_args() == 0)
		{
			return $this->auto_filter;
		}

		$this->auto_filter = $filter;

		return $this;
	}


	/**
	 * Sets the view filename.
	 *
	 *     $view->set_filename($file);
	 *
	 * @param   string  view filename
	 * @return  View
	 * @throws  FuelException
	 */
	public function set_filename($file)
	{
		// set find_file's one-time-only search paths
		\Finder::instance()->flash($this->request_paths);

		// locate the view file
		if (($path = \Finder::search('views', $file, '.'.$this->extension, false, false)) === false)
		{
			throw new \FuelException('The requested view could not be found: '.\Fuel::clean_path($file));
		}

		// Store the file path locally
		$this->file_name = $path;

		return $this;
	}

	/**
	 * Searches for the given variable and returns its value.
	 * Local variables will be returned before global variables.
	 *
	 *     $value = $view->get('foo', 'bar');
	 *
	 * If the key is not given or null, the entire data array is returned.
	 *
	 * If a default parameter is not given and the variable does not
	 * exist, it will throw an OutOfBoundsException.
	 *
	 * @param   string  The variable name
	 * @param   mixed   The default value to return
	 * @return  mixed
	 * @throws  OutOfBoundsException
	 */
	public function &get($key = null, $default = null)
	{
		if (func_num_args() === 0 or $key === null)
		{
			return $this->data;
		}
		elseif (array_key_exists($key, $this->data))
		{
			return $this->data[$key];
		}
		elseif (array_key_exists($key, static::$global_data))
		{
			return static::$global_data[$key];
		}

		if (is_null($default) and func_num_args() === 1)
		{
			throw new \OutOfBoundsException('View variable is not set: '.$key);
		}
		else
		{
			// assign it first, you can't return a return value by reference directly!
			$default = \Fuel::value($default);
			return $default;
		}
	}

	/**
	 * Assigns a variable by name. Assigned values will be available as a
	 * variable within the view file:
	 *
	 *     // This value can be accessed as $foo within the view
	 *     $view->set('foo', 'my value');
	 *
	 * You can also use an array to set several values at once:
	 *
	 *     // Create the values $food and $beverage in the view
	 *     $view->set(array('food' => 'bread', 'beverage' => 'water'));
	 *
	 * @param   string   variable name or an array of variables
	 * @param   mixed    value
	 * @param   bool     whether to filter the data or not
	 * @return  $this
	 */
	public function set($key, $value = null, $filter = null)
	{
		if (is_array($key))
		{
			foreach ($key as $name => $value)
			{
				if ($filter !== null)
				{
					$this->local_filter[$name] = $filter;
				}
				$this->data[$name] = $value;
			}
		}
		else
		{
			if ($filter !== null)
			{
				$this->local_filter[$key] = $filter;
			}
			$this->data[$key] = $value;
		}

		return $this;
	}

	/**
	 * The same as set(), except this defaults to not-encoding the variable
	 * on output.
	 *
	 *     $view->set_safe('foo', 'bar');
	 *
	 * @param   string   variable name or an array of variables
	 * @param   mixed    value
	 * @return  $this
	 */
	public function set_safe($key, $value = null)
	{
		return $this->set($key, $value, false);
	}

	/**
	 * Assigns a value by reference. The benefit of binding is that values can
	 * be altered without re-setting them. It is also possible to bind variables
	 * before they have values. Assigned values will be available as a
	 * variable within the view file:
	 *
	 *     // This reference can be accessed as $ref within the view
	 *     $view->bind('ref', $bar);
	 *
	 * @param   string   variable name
	 * @param   mixed    referenced variable
	 * @param   bool     Whether to filter the var on output
	 * @return  $this
	 */
	public function bind($key, &$value, $filter = null)
	{
		if ($filter !== null)
		{
			$this->local_filter[$key] = $filter;
		}
		$this->data[$key] =& $value;

		return $this;
	}

	/**
	 * Renders the view object to a string. Global and local data are merged
	 * and extracted to create local variables within the view file.
	 *
	 *     $output = $view->render();
	 *
	 * [!!] Global variables with the same key name as local variables will be
	 * overwritten by the local variable.
	 *
	 * @param    string  view filename
	 * @return   string
	 * @throws   FuelException
	 * @uses     static::capture
	 */
	public function render($file = null)
	{
		// reactivate the correct request
		if (class_exists('Request', false))
		{
			$current_request = \Request::active();
			\Request::active($this->active_request);
		}

		// store the current language, and set the correct render language
		if ($this->active_language)
		{
			$current_language = \Config::get('language', 'en');
			\Config::set('language', $this->active_language);
		}

		// override the view filename if needed
		if ($file !== null)
		{
			$this->set_filename($file);
		}

		// and make sure we have one
		if (empty($this->file_name))
		{
			throw new \FuelException('You must set the file to use within your view before rendering');
		}

		// combine local and global data and capture the output
		$return = $this->process_file();

		// restore the current language setting
		$this->active_language and \Config::set('language', $current_language);

		// and the active request class
		if (class_exists('Request', false))
		{
			\Request::active($current_request);
		}

		return $return;
	}

}
