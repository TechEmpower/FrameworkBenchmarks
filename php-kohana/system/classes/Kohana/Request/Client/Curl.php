<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * [Request_Client_External] Curl driver performs external requests using the
 * php-curl extention. This is the default driver for all external requests.
 * 
 * @package    Kohana
 * @category   Base
 * @author     Kohana Team
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 * @uses       [PHP cURL](http://php.net/manual/en/book.curl.php)
 */
class Kohana_Request_Client_Curl extends Request_Client_External {

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
		// Response headers
		$response_headers = array();

		$options = array();

		// Set the request method
		$options = $this->_set_curl_request_method($request, $options);

		// Set the request body. This is perfectly legal in CURL even
		// if using a request other than POST. PUT does support this method
		// and DOES NOT require writing data to disk before putting it, if
		// reading the PHP docs you may have got that impression. SdF
		$options[CURLOPT_POSTFIELDS] = $request->body();

		// Process headers
		if ($headers = $request->headers())
		{
			$http_headers = array();

			foreach ($headers as $key => $value)
			{
				$http_headers[] = $key.': '.$value;
			}

			$options[CURLOPT_HTTPHEADER] = $http_headers;
		}

		// Process cookies
		if ($cookies = $request->cookie())
		{
			$options[CURLOPT_COOKIE] = http_build_query($cookies, NULL, '; ');
		}

		// Get any exisiting response headers
		$response_header = $response->headers();

		// Implement the standard parsing parameters
		$options[CURLOPT_HEADERFUNCTION]        = array($response_header, 'parse_header_string');
		$this->_options[CURLOPT_RETURNTRANSFER] = TRUE;
		$this->_options[CURLOPT_HEADER]         = FALSE;

		// Apply any additional options set to 
		$options += $this->_options;

		$uri = $request->uri();

		if ($query = $request->query())
		{
			$uri .= '?'.http_build_query($query, NULL, '&');
		}

		// Open a new remote connection
		$curl = curl_init($uri);

		// Set connection options
		if ( ! curl_setopt_array($curl, $options))
		{
			throw new Request_Exception('Failed to set CURL options, check CURL documentation: :url',
				array(':url' => 'http://php.net/curl_setopt_array'));
		}

		// Get the response body
		$body = curl_exec($curl);

		// Get the response information
		$code = curl_getinfo($curl, CURLINFO_HTTP_CODE);

		if ($body === FALSE)
		{
			$error = curl_error($curl);
		}

		// Close the connection
		curl_close($curl);

		if (isset($error))
		{
			throw new Request_Exception('Error fetching remote :url [ status :code ] :error',
				array(':url' => $request->url(), ':code' => $code, ':error' => $error));
		}

		$response->status($code)
			->body($body);

		return $response;
	}

	/**
	 * Sets the appropriate curl request options. Uses the responding options
	 * for POST and PUT, uses CURLOPT_CUSTOMREQUEST otherwise
	 * @param Request $request
	 * @param array $options
	 * @return array
	 */
	public function _set_curl_request_method(Request $request, array $options)
	{
		switch ($request->method()) {
			case Request::POST:
				$options[CURLOPT_POST] = TRUE;
				break;
			case Request::PUT:
				$options[CURLOPT_PUT] = TRUE;
				break;
			default:
				$options[CURLOPT_CUSTOMREQUEST] = $request->method();
				break;
		}
		return $options;
	}

} // End Kohana_Request_Client_Curl