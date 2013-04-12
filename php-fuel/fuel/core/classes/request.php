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
 * The Request class is used to create and manage new and existing requests.  There
 * is a main request which comes in from the browser or command line, then new
 * requests can be created for HMVC requests.
 *
 * Example Usage:
 *
 *     $request = Request::forge('foo/bar')->execute();
 *     echo $request->response();
 *
 * @package     Fuel
 * @subpackage  Core
 */
class Request
{

	/**
	 * Holds the main request instance
	 *
	 * @var  Request
	 */
	protected static $main = false;

	/**
	 * Holds the global active request instance
	 *
	 * @var  Request
	 */
	protected static $active = false;

	/**
	 * Generates a new request.  The request is then set to be the active
	 * request.  If this is the first request, then save that as the main
	 * request for the app.
	 *
	 * Usage:
	 *
	 *     Request::forge('hello/world');
	 *
	 * @param   string   The URI of the request
	 * @param   mixed    Internal: whether to use the routes; external: driver type or array with settings (driver key must be set)
	 * @param   string   request method
	 * @return  Request  The new request object
	 */
	public static function forge($uri = null, $options = true, $method = null)
	{
		is_bool($options) and $options = array('route' => $options);
		is_string($options) and $options = array('driver' => $options);

		if ( ! empty($options['driver']))
		{
			$class = \Inflector::words_to_upper('Request_'.$options['driver']);
			return $class::forge($uri, $options, $method);
		}

		$request = new static($uri, isset($options['route']) ? $options['route'] : true, $method);
		if (static::$active)
		{
			$request->parent = static::$active;
			static::$active->children[] = $request;
		}

		// fire any request created events
		\Event::instance()->has_events('request_created') and \Event::instance()->trigger('request_created', '', 'none');

		return $request;
	}

	/**
	 * Returns the main request instance (the one from the browser or CLI).
	 * This is the first executed Request, not necessarily the root parent of the current request.
	 *
	 * Usage:
	 *
	 *     Request::main();
	 *
	 * @return  Request
	 */
	public static function main()
	{
		return static::$main;
	}

	/**
	 * Returns the active request currently being used.
	 *
	 * Usage:
	 *
	 *     Request::active();
	 *
	 * @param   Request|null|false  overwrite current request before returning, false prevents overwrite
	 * @return  Request
	 */
	public static function active($request = false)
	{
		if ($request !== false)
		{
			static::$active = $request;
		}

		return static::$active;
	}

	/**
	 * Returns the current request is an HMVC request
	 *
	 * Usage:
	 *
	 *     if (Request::is_hmvc())
	 *     {
	 *         // Do something special...
	 *         return;
	 *     }
	 *
	 * @return  bool
	 */
	public static function is_hmvc()
	{
		return ((\Fuel::$is_cli and static::main()) or static::active() !== static::main());
	}

	/**
	 * Reset's the active request with the previous one.  This is needed after
	 * the active request is finished.
	 *
	 * Usage:
	 *
	 *    Request::reset_request();
	 *
	 * @return  void
	 */
	public static function reset_request()
	{
		// Let's make the previous Request active since we are done executing this one.
		static::$active = static::$active->parent();
	}


	/**
	 * Holds the response object of the request.
	 *
	 * @var  Response
	 */
	public $response = null;

	/**
	 * The Request's URI object.
	 *
	 * @var  Uri
	 */
	public $uri = null;

	/**
	 * The request's route object
	 *
	 * @var  Route
	 */
	public $route = null;

	/**
	 * @var  string  $method  request method
	 */
	protected $method = null;

	/**
	 * The current module
	 *
	 * @var  string
	 */
	public $module = '';

	/**
	 * The current controller directory
	 *
	 * @var  string
	 */
	public $directory = '';

	/**
	 * The request's controller
	 *
	 * @var  string
	 */
	public $controller = '';

	/**
	 * The request's controller action
	 *
	 * @var  string
	 */
	public $action = '';

	/**
	 * The request's method params
	 *
	 * @var  array
	 */
	public $method_params = array();

	/**
	 * The request's named params
	 *
	 * @var  array
	 */
	public $named_params = array();

	/**
	 * Controller instance once instantiated
	 *
	 * @var  Controller
	 */
	public $controller_instance;

	/**
	 * Search paths for the current active request
	 *
	 * @var  array
	 */
	public $paths = array();

	/**
	 * Request that created this one
	 *
	 * @var  Request
	 */
	protected $parent = null;

	/**
	 * Requests created by this request
	 *
	 * @var  array
	 */
	protected $children = array();

	/**
	 * Creates the new Request object by getting a new URI object, then parsing
	 * the uri with the Route class.
	 *
	 * Usage:
	 *
	 *     $request = new Request('foo/bar');
	 *
	 * @param   string  the uri string
	 * @param   bool    whether or not to route the URI
	 * @param   string  request method
	 * @return  void
	 */
	public function __construct($uri, $route = true, $method = null)
	{
		$this->uri = new \Uri($uri);
		$this->method = $method ?: \Input::method();

		logger(\Fuel::L_INFO, 'Creating a new Request with URI = "'.$this->uri->get().'"', __METHOD__);

		// check if a module was requested
		if (count($this->uri->get_segments()) and $module_path = \Module::exists($this->uri->get_segment(1)))
		{
			// check if the module has routes
			if (is_file($module_path .= 'config/routes.php'))
			{
				$module = $this->uri->get_segment(1);

				// load and add the module routes
				$module_routes = \Fuel::load($module_path);

				$prepped_routes = array();
				foreach($module_routes as $name => $_route)
				{
					if ($name === '_root_')
					{
						$name = $module;
					}
					elseif (strpos($name, $module.'/') !== 0 and $name != $module and $name !== '_404_')
					{
						$name = $module.'/'.$name;
					}

					$prepped_routes[$name] = $_route;
				};

				// update the loaded list of routes
				\Router::add($prepped_routes, null, true);
			}
		}

		$this->route = \Router::process($this, $route);

		if ( ! $this->route)
		{
			return;
		}

		$this->module = $this->route->module;
		$this->controller = $this->route->controller;
		$this->action = $this->route->action;
		$this->method_params = $this->route->method_params;
		$this->named_params = $this->route->named_params;

		if ($this->route->module !== null)
		{
			$this->add_path(\Module::exists($this->module));
		}
	}

	/**
	 * This executes the request and sets the output to be used later.
	 *
	 * Usage:
	 *
	 *     $request = Request::forge('hello/world')->execute();
	 *
	 * @param  array|null  $method_params  An array of parameters to pass to the method being executed
	 * @return  Request  This request object
	 */
	public function execute($method_params = null)
	{
		// fire any request started events
		\Event::instance()->has_events('request_started') and \Event::instance()->trigger('request_started', '', 'none');

		if (\Fuel::$profiling)
		{
			\Profiler::mark(__METHOD__.' Start');
		}

		logger(\Fuel::L_INFO, 'Called', __METHOD__);

		// Make the current request active
		static::$active = $this;

		// First request called is also the main request
		if ( ! static::$main)
		{
			logger(\Fuel::L_INFO, 'Setting main Request', __METHOD__);
			static::$main = $this;
		}

		if ( ! $this->route)
		{
			static::reset_request();
			throw new \HttpNotFoundException();
		}

		// save the current language so we can restore it after the call
		$current_language = \Config::get('language', 'en');

		try
		{
			if ($this->route->callable !== null)
			{
				$response = call_user_func_array($this->route->callable, array($this));

				if ( ! $response instanceof Response)
				{
					$response = new \Response($response);
				}
			}
			else
			{
				$method_prefix = $this->method.'_';
				$class = $this->controller;

				// Allow override of method params from execute
				if (is_array($method_params))
				{
					$this->method_params = array_merge($this->method_params, $method_params);
				}

				// If the class doesn't exist then 404
				if ( ! class_exists($class))
				{
					throw new \HttpNotFoundException();
				}

				// Load the controller using reflection
				$class = new \ReflectionClass($class);

				if ($class->isAbstract())
				{
					throw new \HttpNotFoundException();
				}

				// Create a new instance of the controller
				$this->controller_instance = $class->newInstance($this);

				$this->action = $this->action ?: ($class->hasProperty('default_action') ? $class->getProperty('default_action')->getValue($this->controller_instance) : 'index');
				$method = $method_prefix.$this->action;

				// Allow to do in controller routing if method router(action, params) exists
				if ($class->hasMethod('router'))
				{
					$method = 'router';
					$this->method_params = array($this->action, $this->method_params);
				}

				if ( ! $class->hasMethod($method))
				{
					$method = 'action_'.$this->action;
				}

				if ($class->hasMethod($method))
				{
					$action = $class->getMethod($method);

					if ( ! $action->isPublic())
					{
						throw new \HttpNotFoundException();
					}

					// fire any controller started events
					\Event::instance()->has_events('controller_started') and \Event::instance()->trigger('controller_started', '', 'none');

					$class->hasMethod('before') and $class->getMethod('before')->invoke($this->controller_instance);

					$response = $action->invokeArgs($this->controller_instance, $this->method_params);

					$class->hasMethod('after') and $response = $class->getMethod('after')->invoke($this->controller_instance, $response);

					// fire any controller finished events
					\Event::instance()->has_events('controller_finished') and \Event::instance()->trigger('controller_finished', '', 'none');
				}
				else
				{
					throw new \HttpNotFoundException();
				}
			}

			// restore the language setting
			\Config::set('language', $current_language);
		}
		catch (\Exception $e)
		{
			static::reset_request();

			// restore the language setting
			\Config::set('language', $current_language);

			throw $e;
		}

		// Get the controller's output
		if ($response instanceof Response)
		{
			$this->response = $response;
		}
		else
		{
			throw new \FuelException(get_class($this->controller_instance).'::'.$method.'() or the controller after() method must return a Response object.');
		}

		// fire any request finished events
		\Event::instance()->has_events('request_finished') and \Event::instance()->trigger('request_finished', '', 'none');

		if (\Fuel::$profiling)
		{
			\Profiler::mark(__METHOD__.' End');
		}

		static::reset_request();

		return $this;
	}

	/**
	 * Sets the request method.
	 *
	 * @param   string  $method  request method
	 * @return  object  current instance
	 */
	public function set_method($method)
	{
		$this->method = strtoupper($method);
		return $this;
	}

	/**
	 * Returns the request method.
	 *
	 * @return  string  request method
	 */
	public function get_method()
	{
		return $this->method;
	}

	/**
	 * Gets this Request's Response object;
	 *
	 * Usage:
	 *
	 *     $response = Request::forge('foo/bar')->execute()->response();
	 *
	 * @return  Response  This Request's Response object
	 */
	public function response()
	{
		return $this->response;
	}

	/**
	 * Returns the Request that created this one
	 *
	 * @return  Request|null
	 */
	public function parent()
	{
		return $this->parent;
	}

	/**
	 * Returns an array of Requests created by this one
	 *
	 * @return  array
	 */
	public function children()
	{
		return $this->children;
	}

	/**
	 * Add to paths which are used by Finder::search()
	 *
	 * @param   string  the new path
	 * @param   bool    whether to add to the front or the back of the array
	 * @return  void
	 */
	public function add_path($path, $prefix = false)
	{
		if ($prefix)
		{
			// prefix the path to the paths array
			array_unshift($this->paths, $path);
		}
		else
		{
			// add the new path
			$this->paths[] = $path;
		}
	}

	/**
	 * Returns the array of currently loaded search paths.
	 *
	 * @return  array  the array of paths
	 */
	public function get_paths()
	{
		return $this->paths;
	}

	/**
	 * Gets a specific named parameter
	 *
	 * @param   string  $param    Name of the parameter
	 * @param   mixed   $default  Default value
	 * @return  mixed
	 */
	public function param($param, $default = null)
	{
		if ( ! isset($this->named_params[$param]))
		{
			return \Fuel::value($default);
		}

		return $this->named_params[$param];
	}

	/**
	 * Gets all of the named parameters
	 *
	 * @return  array
	 */
	public function params()
	{
		return $this->named_params;
	}

	/**
	 * PHP magic function returns the Output of the request.
	 *
	 * Usage:
	 *
	 *     $request = Request::forge('hello/world')->execute();
	 *     echo $request;
	 *
	 * @return  string  the response
	 */
	public function __toString()
	{
		return (string) $this->response;
	}
}
