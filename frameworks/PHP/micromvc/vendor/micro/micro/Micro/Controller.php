<?php
/**
 * Controller
 *
 * Basic outline for standard system controllers.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

abstract class Controller
{
	// URL path segment matched to route here
	public $route = NULL;

	// The dispatch object (Can be used to load other Controllers)
	public $dispatch = NULL;

	/**
	 * Set error handling and start session
	 */
	public function __construct($route, \Micro\Dispatch $dispatch)
	{
		$this->route = $route;
		$this->dispatch = $dispatch;
	}


	/**
	 * Called before the controller method is run
	 *
	 * @param string $method name that will be run
	 */
	public function initialize($method) {}


	/* HTTP Request Methods
	abstract public function run();		// Default for all non-defined request methods
	abstract public function get();
	abstract public function post();
	abstract public function put();
	abstract public function delete();
	abstract public function options();
	abstract public function head();
	*/

	/**
	 * Called after the controller method is run to send the response
	 */
	public function send() {}

}

// End
