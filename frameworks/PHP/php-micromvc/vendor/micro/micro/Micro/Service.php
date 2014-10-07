<?php
/**
 * Service Locator
 *
 * Responsible for tracking objects required by other classes in a
 * registry-based, dependency injector fashion.
 *
 * $service = new Service();
 *
 * $service->db = function($service, $config)
 * {
 * 		return new DB($config);
 * }
 *
 * // Create Database object
 * $service->db(config('database'));
 *
 * // Use newly created database object
 * $service->db()->query('SELECT * FROM table');
 *
 * Another example is classes which have dependencies on other classes.
 *
 * // Create Error Object Instructions
 * $service->error = function($service)
 * {
 *  	return new Error($service->log());
 * };
 *
 * // Create Log Object Instructions
 * $service->log = function($service)
 * {
 *  	return new File_Log();
 * };
 *
 * // Creates Error and File_Log classes, return Error instance, then report this error
 * $service->error->report('This will be logged');
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

class Service
{

	protected $s = array();

	/**
	 * Set an object or closure
	 *
	 * @param string $key name
	 * @param mixed $callable closure or object instance
	 */
	function __set($key, $callable)
	{
		// Like normal PHP, property/method names should be case-insensitive.
		$key = strtolower($key);

		// Simple object storage?
		if( ! $callable instanceof \Closure)
		{
			$this->s[$key] = $callable;
			return;
		}

		// Create singleton wrapper function tied to this service object
		$this->s[$key] = function ($c, array $arg) use ($callable)
		{
			static $object;
			if (is_null($object))
			{
				array_unshift($arg, $c);
				$object = call_user_func_array($callable, $arg);
			}
			return $object;
		};
	}


	/**
	 * Fetch an object or closure
	 *
	 * @param string $key name
	 * @return mixed
	 */
	function __get($key)
	{
		return $this->s[$key];
	}

	/**
	 * Check that the given key name exists
	 *
	 * @param string $key name
	 * @return mixed
	 */
	function __isset($key)
	{
		return isset($this->s[$key]);
	}


	/**
	 * Call the given closure singleton function
	 *
	 * @param string $key name
	 * @param array $arg for closure
	 * @return mixed
	 */
	function __call($key, $arg)
	{
		return $this->s[$key]($this, $arg);
	}

}
