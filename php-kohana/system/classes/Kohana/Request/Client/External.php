<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * [Request_Client_External] provides a wrapper for all external request
 * processing. This class should be extended by all drivers handling external
 * requests.
 * 
 * Supported out of the box:
 *  - Curl (default)
 *  - PECL HTTP
 *  - Streams
 * 
 * To select a specific external driver to use as the default driver, set the
 * following property within the Application bootstrap. Alternatively, the
 * client can be injected into the request object.
 * 
 * @example
 * 
 *       // In application bootstrap
 *       Request_Client_External::$client = 'Request_Client_Stream';
 * 
 *       // Add client to request
 *       $request = Request::factory('http://some.host.tld/foo/bar')
 *           ->client(Request_Client_External::factory('Request_Client_HTTP));
 * 
 * @package    Kohana
 * @category   Base
 * @author     Kohana Team
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 * @uses       [PECL HTTP](http://php.net/manual/en/book.http.php)
 */
abstract class Kohana_Request_Client_External extends Request_Client {

	/**
	 * Use:
	 *  - Request_Client_Curl (default)
	 *  - Request_Client_HTTP
	 *  - Request_Client_Stream
	 * 
	 * @var     string    defines the external client to use by default
	 */
	public static $client = 'Request_Client_Curl';

	/**
	 * Factory method to create a new Request_Client_External object based on
	 * the client name passed, or defaulting to Request_Client_External::$client
	 * by default.
	 * 
	 * Request_Client_External::$client can be set in the application bootstrap.
	 *
	 * @param   array   $params parameters to pass to the client
	 * @param   string  $client external client to use
	 * @return  Request_Client_External
	 * @throws  Request_Exception
	 */
	public static function factory(array $params = array(), $client = NULL)
	{
		if ($client === NULL)
		{
			$client = Request_Client_External::$client;
		}

		$client = new $client($params);

		if ( ! $client instanceof Request_Client_External)
		{
			throw new Request_Exception('Selected client is not a Request_Client_External object.');
		}

		return $client;
	}

	/**
	 * @var     array     curl options
	 * @link    http://www.php.net/manual/function.curl-setopt
	 * @link    http://www.php.net/manual/http.request.options
	 */
	protected $_options = array();

	/**
	 * Processes the request, executing the controller action that handles this
	 * request, determined by the [Route].
	 *
	 * 1. Before the controller action is called, the [Controller::before] method
	 * will be called.
	 * 2. Next the controller action will be called.
	 * 3. After the controller action is called, the [Controller::after] method
	 * will be called.
	 *
	 * By default, the output from the controller is captured and returned, and
	 * no headers are sent.
	 *
	 *     $request->execute();
	 *
	 * @param   Request   $request   A request object
	 * @param   Response  $response  A response object
	 * @return  Response
	 * @throws  Kohana_Exception
	 * @uses    [Kohana::$profiling]
	 * @uses    [Profiler]
	 */
	public function execute_request(Request $request, Response $response)
	{
		if (Kohana::$profiling)
		{
			// Set the benchmark name
			$benchmark = '"'.$request->uri().'"';

			if ($request !== Request::$initial AND Request::$current)
			{
				// Add the parent request uri
				$benchmark .= ' « "'.Request::$current->uri().'"';
			}

			// Start benchmarking
			$benchmark = Profiler::start('Requests', $benchmark);
		}

		// Store the current active request and replace current with new request
		$previous = Request::$current;
		Request::$current = $request;

		// Resolve the POST fields
		if ($post = $request->post())
		{
			$request->body(http_build_query($post, NULL, '&'))
				->headers('content-type', 'application/x-www-form-urlencoded');
		}

		// If Kohana expose, set the user-agent
		if (Kohana::$expose)
		{
			$request->headers('user-agent', Kohana::version());
		}

		try
		{
			$response = $this->_send_message($request, $response);
		}
		catch (Exception $e)
		{
			// Restore the previous request
			Request::$current = $previous;

			if (isset($benchmark))
			{
				// Delete the benchmark, it is invalid
				Profiler::delete($benchmark);
			}

			// Re-throw the exception
			throw $e;
		}

		// Restore the previous request
		Request::$current = $previous;

		if (isset($benchmark))
		{
			// Stop the benchmark
			Profiler::stop($benchmark);
		}

		// Return the response
		return $response;
	}

	/**
	 * Set and get options for this request.
	 *
	 * @param   mixed    $key    Option name, or array of options
	 * @param   mixed    $value  Option value
	 * @return  mixed
	 * @return  Request_Client_External
	 */
	public function options($key = NULL, $value = NULL)
	{
		if ($key === NULL)
			return $this->_options;

		if (is_array($key))
		{
			$this->_options = $key;
		}
		elseif ($value === NULL)
		{
			return Arr::get($this->_options, $key);
		}
		else
		{
			$this->_options[$key] = $value;
		}

		return $this;
	}

	/**
	 * Sends the HTTP message [Request] to a remote server and processes
	 * the response.
	 *
	 * @param   Request   $request    Request to send
	 * @param   Response  $response   Response to send
	 * @return  Response
	 */
	abstract protected function _send_message(Request $request, Response $response);

} // End Kohana_Request_Client_External