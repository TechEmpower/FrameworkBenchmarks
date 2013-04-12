<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * [Request_Client_External] HTTP driver performs external requests using the
 * php-http extention. To use this driver, ensure the following is completed
 * before executing an external request- ideally in the application bootstrap.
 * 
 * @example
 * 
 *       // In application bootstrap
 *       Request_Client_External::$client = 'Request_Client_HTTP';
 * 
 * @package    Kohana
 * @category   Base
 * @author     Kohana Team
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 * @uses       [PECL HTTP](http://php.net/manual/en/book.http.php)
 */
class Kohana_Request_Client_HTTP extends Request_Client_External {

	/**
	 * Creates a new `Request_Client` object,
	 * allows for dependency injection.
	 *
	 * @param   array    $params Params
	 * @throws  Request_Exception
	 */
	public function __construct(array $params = array())
	{
		// Check that PECL HTTP supports requests
		if ( ! http_support(HTTP_SUPPORT_REQUESTS))
		{
			throw new Request_Exception('Need HTTP request support!');
		}

		// Carry on
		parent::__construct($params);
	}

	/**
	 * @var     array     curl options
	 * @link    http://www.php.net/manual/function.curl-setopt
	 */
	protected $_options = array();

	/**
	 * Sends the HTTP message [Request] to a remote server and processes
	 * the response.
	 *
	 * @param   Request   $request  request to send
	 * @param   Response  $request  response to send
	 * @return  Response
	 */
	public function _send_message(Request $request, Response $response)
	{
		$http_method_mapping = array(
			HTTP_Request::GET     => HTTPRequest::METH_GET,
			HTTP_Request::HEAD    => HTTPRequest::METH_HEAD,
			HTTP_Request::POST    => HTTPRequest::METH_POST,
			HTTP_Request::PUT     => HTTPRequest::METH_PUT,
			HTTP_Request::DELETE  => HTTPRequest::METH_DELETE,
			HTTP_Request::OPTIONS => HTTPRequest::METH_OPTIONS,
			HTTP_Request::TRACE   => HTTPRequest::METH_TRACE,
			HTTP_Request::CONNECT => HTTPRequest::METH_CONNECT,
		);

		// Create an http request object
		$http_request = new HTTPRequest($request->uri(), $http_method_mapping[$request->method()]);

		if ($this->_options)
		{
			// Set custom options
			$http_request->setOptions($this->_options);
		}

		// Set headers
		$http_request->setHeaders($request->headers()->getArrayCopy());

		// Set cookies
		$http_request->setCookies($request->cookie());

		// Set query data (?foo=bar&bar=foo)
		$http_request->setQueryData($request->query());

		// Set the body
		if ($request->method() == HTTP_Request::PUT)
		{
			$http_request->addPutData($request->body());
		}
		else
		{
			$http_request->setBody($request->body());
		}

		try
		{
			$http_request->send();
		}
		catch (HTTPRequestException $e)
		{
			throw new Request_Exception($e->getMessage());
		}
		catch (HTTPMalformedHeaderException $e)
		{
			throw new Request_Exception($e->getMessage());
		}
		catch (HTTPEncodingException $e)
		{
			throw new Request_Exception($e->getMessage());
		}

		// Build the response
		$response->status($http_request->getResponseCode())
			->headers($http_request->getResponseHeader())
			->cookie($http_request->getResponseCookies())
			->body($http_request->getResponseBody());

		return $response;
	}

} // End Kohana_Request_Client_HTTP
