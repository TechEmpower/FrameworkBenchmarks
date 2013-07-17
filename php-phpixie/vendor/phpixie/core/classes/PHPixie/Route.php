<?php

namespace PHPixie;

/**
 * Routing class to extract and parse request parameters from the URL.
 * @package Core
 */
class Route
{

	/**
	 * Name of the route.
	 * @var string
	 */
	public $name;

	/**
	 * Rule for this route.
	 * @var mixed
	 */
	public $rule;

	/**
	 * Default parameters for this route.
	 * @var mixed
	 */
	public $defaults;

	/**
	 * Methods to restrict this route to.
	 * @var array
	 */
	public $methods;

	/**
	 * Associative array of route instances.
	 * @var array
	 */
	private static $routes = array();

	/**
	 * Constructs a route.
	 *
	 * @param string $name Name of the route
	 * @param mixed $rule Rule for this route
	 * @param array $defaults Default parameters for the route
	 * @param mixed $methods Methods to restrict this route to.
	 *                       Either a single method or an array of them.
	 */
	public function __construct($name, $rule, $defaults, $methods = null)
	{
		$this->name = $name;
		$this->rule = $rule;
		$this->defaults = $defaults;
		if($methods != null){
			if (is_string($methods))
				$methods = array($methods);
			$methods = array_map('strtoupper', $methods);
		}
		$this->methods = $methods;
	}

	/**
	 * Generates a url for a route
	 *
	 * @param array $params    Parameters to substitute in the route
	 * @param bool $absolute   Whether to return an absolute url
	 * @param string $protocol	Protocol to use for absolute url
	 * @return string Generated url
	 */
	public function url($params = array(), $absolute = false, $protocol = 'http')
	{
		if (is_callable($this->rule))
			throw new \Exception("The rule for '{$this->name}' route is a function and cannot be reversed");

		$url = is_array($this->rule) ? $this->rule[0] : $this->rule;

		$replace = array();
		$params = array_merge($this->defaults, $params);
		foreach ($params as $key => $value)
			$replace["<{$key}>"] = $value;
		$url = str_replace(array_keys($replace), array_values($replace), $url);

		$count = 1;
		$chars = '[^\(\)]*?';
		while ($count > 0)
			$url = preg_replace("#\({$chars}<{$chars}>{$chars}\)#", '', $url, -1, $count);

		$url = str_replace(array('(', ')'), '', $url);

		if ($absolute)
			$url = $protocol.'://'.$_SERVER['HTTP_HOST'].$url;

		return $url;
	}

}
