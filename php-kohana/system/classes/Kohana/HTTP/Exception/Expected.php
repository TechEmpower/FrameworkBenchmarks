<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * "Expected" HTTP exception class. Used for all [HTTP_Exception]'s where a standard
 * Kohana error page should never be shown. 
 * 
 * Eg [HTTP_Exception_301], [HTTP_Exception_302] etc
 *
 * @package    Kohana
 * @category   Exceptions
 * @author     Kohana Team
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
abstract class Kohana_HTTP_Exception_Expected extends HTTP_Exception {

	/**
	 * @var  Response   Response Object
	 */
	protected $_response;

	/**
	 * Creates a new translated exception.
	 *
	 *     throw new Kohana_Exception('Something went terrible wrong, :user',
	 *         array(':user' => $user));
	 *
	 * @param   string  $message    status message, custom content to display with error
	 * @param   array   $variables  translation variables
	 * @return  void
	 */
	public function __construct($message = NULL, array $variables = NULL, Exception $previous = NULL)
	{
		parent::__construct($message, $variables, $previous);

		// Prepare our response object and set the correct status code.
		$this->_response = Response::factory()
			->status($this->_code);
	}

	/**
	 * Gets and sets headers to the [Response].
	 * 
	 * @see     [Response::headers]
	 * @param   mixed   $key
	 * @param   string  $value
	 * @return  mixed
	 */
	public function headers($key = NULL, $value = NULL)
	{
		$result = $this->_response->headers($key, $value);

		if ( ! $result instanceof Response)
			return $result;

		return $this;
	}

	/**
	 * Validate this exception contains everything needed to continue.
	 * 
	 * @throws Kohana_Exception
	 * @return bool
	 */
	public function check()
	{
		return TRUE;
	}

	/**
	 * Generate a Response for the current Exception
	 * 
	 * @uses   Kohana_Exception::response()
	 * @return Response
	 */
	public function get_response()
	{
		$this->check();

		return $this->_response;
	}

} // End Kohana_HTTP_Exception