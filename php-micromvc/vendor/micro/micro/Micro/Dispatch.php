<?php
/**
 * Dispatch
 *
 * Dispatch controllers based on routes
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

class Dispatch
{
	public $routes;

	public function __construct(array $routes)
	{
		$this->routes = $routes;
	}

	public function controller($path, $method)
	{
		// Parse the routes to find the correct controller
		list($params, $route, $controller) = $this->route($path);

		// Load and run action
		$controller = new $controller($route, $this);

		// We are ignoring TRACE & CONNECT
		$request_methods = array('GET', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'HEAD');

		// Look for a RESTful method, or try the default run()
		if( ! in_array($method, $request_methods) OR ! method_exists($controller, $method))
		{
			if( ! method_exists($controller, 'run'))
			{
				throw new \Exception('Invalid Request Method.');
			}

			$method = 'run';
		}

		// Controller setup here
		$controller->initialize($method);

		if($params)
		{
			call_user_func_array(array($controller, $method), $params);
		}
		else
		{
			$controller->$method();
		}

		// Return the controller instance
		return $controller;
	}


	/**
	 * Parse the given URL path and return the correct controller and parameters.
	 *
	 * @param string $path segment of URL
	 * @param array $routes to test against
	 * @return array
	 */
	public function route($path)
	{
		$path = trim($path, '/');

		// Default homepage route
		if($path === '')
		{
			return array(array(), '', $this->routes['']);
		}

		// If this is not a valid, safe path (more complex params belong in GET/POST)
		if($path AND ! preg_match('/^[\w\-~\/\.]{1,400}$/', $path))
		{
			$path = '404';
		}

		foreach($this->routes as $route => $controller)
		{
			if( ! $route) continue; // Skip homepage route

			// Is this a regex?
			if($route{0} === '/')
			{
				if(preg_match($route, $path, $matches))
				{
					$complete = array_shift($matches);

					// The following code tries to solve:
					// (Regex) "/^path/(\w+)/" + (Path) "path/word/other" = (Params) array(word, other)

					// Skip the regex match and continue from there
					$params = explode('/', trim(mb_substr($path, mb_strlen($complete)), '/'));

					if($params[0])
					{
						// Add captured group back into params
						foreach($matches as $match)
						{
							array_unshift($params, $match);
						}
					}
					else
					{
						$params = $matches;
					}

					//print dump($params, $matches);
					return array($params, $complete, $controller);
				}
			}
			else
			{
				if(mb_substr($path, 0, mb_strlen($route)) === $route)
				{
					$params = explode('/', trim(mb_substr($path, mb_strlen($route)), '/'));
					return array($params, $route, $controller);
				}
			}
		}

		// Controller not found
		return array(array($path), $path, $this->routes['404']);
	}
}
