<?php
namespace PHPixie;

/**
 * Router for matching URLs to corresponding Routes
 * @package Core
 */
class Router {

	/**
	 * Pixie Dependancy Container
	 * @var \PHPixie\Pixie
	 */
	protected $pixie;
	
	/**
	 * Associative array of route instances.
	 * @var array
	 */
	protected $routes = array();
	
	/**
	 * Constructs a router
	 *
	 * @param \PHPixie\Pixie $pixie Pixie dependency container
	 */
	public function __construct($pixie) {
		$this->pixie = $pixie;
	}

	
	/**
	 * Ads a route
	 *
	 * @param string $name     Name of the route. Routes with the same name will override one another.
	 * @param mixed $rule     Either an expression to match URI against or a function that will
	 *                        be passed the URI and must return either an associative array of
	 *                        extracted parameters (if it matches) or False.
	 * @param array   $defaults An associated array of default values.
	 * @return void
	 */
	public function add($route)
	{
		$this->routes[$route->name] = $route;
	}

	/**
	 * Gets route by name
	 *
	 * @param string $name Route name
	 * @return \PHPixie\Route
	 * @throws \Exception If specified route doesn't exist
	 */
	public function get($name)
	{
		if (!isset($this->routes[$name]))
			throw new \Exception("Route {$name} not found.");

		return $this->routes[$name];
	}

	/**
	 * Matches the URI against available routes to find the correct one.
	 *
	 * @param string   $uri Request URI
	 * @param string   $method Request method
	 * @return array Array containing route and matched parameters
	 * @throws \Exception If no route matches the URI
	 * @throws \Exception If route matched but no Controller was defined for it
	 * @throws \Exception If route matched but no action was defined for it
	 */
	public function match($uri, $method = 'GET')
	{
		$matched = false;
		$method = strtoupper($method);
		foreach ($this->routes as $name => $route) {
			if ($route-> methods != null && !in_array($method, $route->methods))
				continue;
			
			$rule = $route->rule;
			if (is_callable($rule))
			{
				if (($data = $rule($uri)) !== FALSE)
				{
					$matched = $name;
					break;
				}
			}
			else
			{
				$pattern = is_array($rule) ? $rule[0] : $rule;
				$pattern = str_replace(')', ')?', $pattern);
				$pixie=$this->pixie;
				$pattern = preg_replace_callback('/<.*?>/', function($str) use ($rule, $pixie) {
						$str = $str[0];
						$regexp = '[a-zA-Z0-9\-\._]+';
						if (is_array($rule))
							$regexp = $pixie->arr($rule[1], str_replace(array('<', '>'), '', $str), $regexp);
						return '(?P'.$str.$regexp.')';
					}, $pattern);

				preg_match('#^'.$pattern.'/?$#', $uri, $match);
				if (!empty($match[0]))
				{
					$matched = $name;
					$data = array();
					foreach ($match as $k => $v)
						if (!is_numeric($k))
							$data[$k] = $v;
					break;
				}
			}
		}
		if ($matched == false)
			throw new \Exception('No route matched your request', 404);
			
		$route = $this->routes[$matched];
		$params = array_merge($route->defaults, $data);
		if (!isset($params['controller']))
			throw new \Exception("Route {$matched} matched, but no controller was defined for this route", 404);
		if (!isset($params['action']))
			throw new \Exception("Route {$matched} matched with controller {$params['controller']}, but no action was defined for this route", 404);

		return array(
					'route'=>$route, 
					'params'=>$params
					);
	}
	
}
