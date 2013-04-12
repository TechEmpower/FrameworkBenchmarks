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
 * Hybrid Controller class
 *
 * A base controller that combines both templated and REST output
 *
 * @package   Fuel
 * @category  Core
 * @author    Fuel Development Team
 */
abstract class Controller_Hybrid extends \Controller_Rest
{

	/**
	* @var string page template
	*/
	public $template = 'template';

	/**
	 * Load the template and create the $this->template object if needed
	 */
	public function before()
	{
		// setup the template if this isn't a RESTful call
		if ( ! $this->is_restful())
		{
			if ( ! empty($this->template) and is_string($this->template))
			{
				// Load the template
				$this->template = \View::forge($this->template);
			}
		}

		return parent::before();
	}

	/**
	 * After controller method has run output the template
	 *
	 * @param  Response  $response
	 */
	public function after($response)
	{
		// return the template if no response is present and this isn't a RESTful call
		if ( ! $this->is_restful())
		{
			// do we have a response passed?
			if(empty($response))
			{
				// maybe one in the rest body?
				$response = $this->response->body;
				if (empty($response))
				{
					// fall back to the defined template
					$response = $this->template;
				}
			}

			if ( ! $response instanceof Response)
			{
				$response = \Response::forge($response);
			}
		}

		return parent::after($response);
	}

	/**
	 * Decide whether to return RESTful or templated response
	 * Override in subclass to introduce custom switching logic.
	 *
	 * @param  boolean
	 */
	public function is_restful()
	{
		return \Input::is_ajax();
	}
}
