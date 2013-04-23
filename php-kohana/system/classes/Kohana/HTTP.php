<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * Contains the most low-level helpers methods in Kohana:
 *
 * - Environment initialization
 * - Locating files within the cascading filesystem
 * - Auto-loading and transparent extension of classes
 * - Variable and path debugging
 *
 * @package    Kohana
 * @category   HTTP
 * @author     Kohana Team
 * @since      3.1.0
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaphp.com/license
 */
abstract class Kohana_HTTP {

	/**
	 * @var  The default protocol to use if it cannot be detected
	 */
	public static $protocol = 'HTTP/1.1';

	/**
	 * Issues a HTTP redirect.
	 *
	 * @param  string    $uri       URI to redirect to
	 * @param  int       $code      HTTP Status code to use for the redirect
	 * @throws HTTP_Exception
	 */
	public static function redirect($uri = '', $code = 302)
	{
		$e = HTTP_Exception::factory($code);

		if ( ! $e instanceof HTTP_Exception_Redirect)
			throw new Kohana_Exception('Invalid redirect code \':code\'', array(
				':code' => $code
			));

		throw $e->location($uri);
	}

	/**
	 * Checks the browser cache to see the response needs to be returned,
	 * execution will halt and a 304 Not Modified will be sent if the
	 * browser cache is up to date.
	 * 
	 * @param  Request   $request   Request
	 * @param  Response  $response  Response
	 * @param  string    $etag      Resource ETag
	 * @throws HTTP_Exception_304
	 * @return Response
	 */
	public static function check_cache(Request $request, Response $response, $etag = NULL)
	{
		// Generate an etag if necessary
		if ($etag == NULL)
		{
			$etag = $response->generate_etag();
		}

		// Set the ETag header
		$response->headers('etag', $etag);

		// Add the Cache-Control header if it is not already set
		// This allows etags to be used with max-age, etc
		if ($response->headers('cache-control'))
		{
			$response->headers('cache-control', $response->headers('cache-control').', must-revalidate');
		}
		else
		{
			$response->headers('cache-control', 'must-revalidate');
		}

		// Check if we have a matching etag
		if ($request->headers('if-none-match') AND (string) $request->headers('if-none-match') === $etag)
		{
			// No need to send data again
			throw HTTP_Exception::factory(304)->headers('etag', $etag);
		}

		return $response;
	}

	/**
	 * Parses a HTTP header string into an associative array
	 *
	 * @param   string   $header_string  Header string to parse
	 * @return  HTTP_Header
	 */
	public static function parse_header_string($header_string)
	{
		// If the PECL HTTP extension is loaded
		if (extension_loaded('http'))
		{
			// Use the fast method to parse header string
			return new HTTP_Header(http_parse_headers($header_string));
		}

		// Otherwise we use the slower PHP parsing
		$headers = array();

		// Match all HTTP headers
		if (preg_match_all('/(\w[^\s:]*):[ ]*([^\r\n]*(?:\r\n[ \t][^\r\n]*)*)/', $header_string, $matches))
		{
			// Parse each matched header
			foreach ($matches[0] as $key => $value)
			{
				// If the header has not already been set
				if ( ! isset($headers[$matches[1][$key]]))
				{
					// Apply the header directly
					$headers[$matches[1][$key]] = $matches[2][$key];
				}
				// Otherwise there is an existing entry
				else
				{
					// If the entry is an array
					if (is_array($headers[$matches[1][$key]]))
					{
						// Apply the new entry to the array
						$headers[$matches[1][$key]][] = $matches[2][$key];
					}
					// Otherwise create a new array with the entries
					else
					{
						$headers[$matches[1][$key]] = array(
							$headers[$matches[1][$key]],
							$matches[2][$key],
						);
					}
				}
			}
		}

		// Return the headers
		return new HTTP_Header($headers);
	}

	/**
	 * Parses the the HTTP request headers and returns an array containing
	 * key value pairs. This method is slow, but provides an accurate
	 * representation of the HTTP request.
	 *
	 *      // Get http headers into the request
	 *      $request->headers = HTTP::request_headers();
	 *
	 * @return  HTTP_Header
	 */
	public static function request_headers()
	{
		// If running on apache server
		if (function_exists('apache_request_headers'))
		{
			// Return the much faster method
			return new HTTP_Header(apache_request_headers());
		}
		// If the PECL HTTP tools are installed
		elseif (extension_loaded('http'))
		{
			// Return the much faster method
			return new HTTP_Header(http_get_request_headers());
		}

		// Setup the output
		$headers = array();

		// Parse the content type
		if ( ! empty($_SERVER['CONTENT_TYPE']))
		{
			$headers['content-type'] = $_SERVER['CONTENT_TYPE'];
		}

		// Parse the content length
		if ( ! empty($_SERVER['CONTENT_LENGTH']))
		{
			$headers['content-length'] = $_SERVER['CONTENT_LENGTH'];
		}

		foreach ($_SERVER as $key => $value)
		{
			// If there is no HTTP header here, skip
			if (strpos($key, 'HTTP_') !== 0)
			{
				continue;
			}

			// This is a dirty hack to ensure HTTP_X_FOO_BAR becomes x-foo-bar
			$headers[str_replace(array('HTTP_', '_'), array('', '-'), $key)] = $value;
		}

		return new HTTP_Header($headers);
	}

	/**
	 * Processes an array of key value pairs and encodes
	 * the values to meet RFC 3986
	 *
	 * @param   array   $params  Params
	 * @return  string
	 */
	public static function www_form_urlencode(array $params = array())
	{
		if ( ! $params)
			return;

		$encoded = array();

		foreach ($params as $key => $value)
		{
			$encoded[] = $key.'='.rawurlencode($value);
		}

		return implode('&', $encoded);
	}
} // End Kohana_HTTP