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

abstract class Controller
{

	/**
	 * @var  Request  The current Request object
	 */
	public $request;

	/**
	 * Sets the controller request object.
	 *
	 * @param   Request   The current request object
	 */
	public function __construct(\Request $request)
	{
		$this->request = $request;
	}

	/**
	 * This method gets called before the action is called
	 */
	public function before() {}

	/**
	 * This method gets called after the action is called
	 */
	public function after($response)
	{
		// Make sure the $response is a Response object
		if ( ! $response instanceof Response)
		{
			$response = \Response::forge($response);
		}

		return $response;
	}

	/**
	 * This method returns the named parameter requested, or all of them
	 * if no parameter is given.
	 *
	 * @param   string  $param    The name of the parameter
	 * @param   mixed   $default  Default value
	 * @return  mixed
	 */
	public function param($param, $default = null)
	{
		return $this->request->param($param, $default);
	}

	/**
	 * This method returns all of the named parameters.
	 *
	 * @return  array
	 */
	public function params()
	{
		return $this->request->params();
	}
}

