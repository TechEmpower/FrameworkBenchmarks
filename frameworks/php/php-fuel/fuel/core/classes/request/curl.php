<?php

namespace Fuel\Core;

class Request_Curl extends \Request_Driver
{
	/**
	 * @var  string  to preserve the original resource url when using get
	 */
	protected $preserve_resource;

	/**
	 * Extends parent constructor to detect availability of cURL
	 *
	 * @param   string  $resource  url to use
	 * @param   array   $options   options array
	 * @param   string  $method    request method
	 * @throws  \RuntimeException
	 */
	public function __construct($resource, array $options, $method = null)
	{

		// check if we have libcurl available
		if ( ! function_exists('curl_init'))
		{
			throw new \RuntimeException('Your PHP installation doesn\'t have cURL enabled. Rebuild PHP with --with-curl');
		}

		logger(\Fuel::L_INFO, 'Creating a new CURL Request with URI = "'.$resource.'"', __METHOD__);

		// If authentication is enabled use it
		if ( ! empty($options['auth']) and ! empty($options['user']) and ! empty($options['pass']))
		{
			$this->http_login($options['user'], $options['pass'], $options['auth']);
		}

		parent::__construct($resource, $options, $method);
	}

	/**
	 * Fetch the connection, create if necessary
	 *
	 * @return  \resource
	 */
	protected function connection()
	{
		// If no a protocol in URL, assume its a local link
		! preg_match('!^\w+://! i', $this->resource) and $this->resource = Uri::create($this->resource);

		return curl_init($this->resource);
	}

	/**
	 * Authenticate to an http server
	 *
	 * @param   string  $username
	 * @param   string  $password
	 * @param   string  $type
	 * @return  Request_Curl
	 */
	public function http_login($username = '', $password = '', $type = 'any')
	{
		$this->set_option(CURLOPT_HTTPAUTH, constant('CURLAUTH_' . strtoupper($type)));
		$this->set_option(CURLOPT_USERPWD, $username . ':' . $password);

		return $this;
	}

	/**
	 * Overwrites driver method to set options driver specifically
	 *
	 * @param   int|string  $code
	 * @param   mixed       $value
	 * @return  Request_Curl
	 */
	public function set_options(array $options)
	{
		foreach ($options as $key => $val)
		{
			if (is_string($key) && ! is_numeric($key))
			{
				$key = constant('CURLOPT_' . strtoupper($key));
			}

			$this->options[$key] = $val;
		}

		return $this;
	}

	public function execute(array $additional_params = array())
	{
		// Reset response
		$this->response = null;
		$this->response_info = array();

		// Set two default options, and merge any extra ones in
		if ( ! isset($this->options[CURLOPT_TIMEOUT]))
		{
			$this->options[CURLOPT_TIMEOUT] = 30;
		}
		if ( ! isset($this->options[CURLOPT_RETURNTRANSFER]))
		{
			$this->options[CURLOPT_RETURNTRANSFER] = true;
		}
		if ( ! isset($this->options[CURLOPT_FAILONERROR]))
		{
			$this->options[CURLOPT_FAILONERROR] = false;
		}

		// Only set follow location if not running securely
		if ( ! ini_get('safe_mode') && ! ini_get('open_basedir'))
		{
			// Ok, follow location is not set already so lets set it to true
			if ( ! isset($this->options[CURLOPT_FOLLOWLOCATION]))
			{
				$this->options[CURLOPT_FOLLOWLOCATION] = true;
			}
		}

		if ( ! empty($this->headers))
		{
			$this->set_option(CURLOPT_HTTPHEADER, $this->get_headers());
		}

		$additional_params and $this->params = \Arr::merge($this->params, $additional_params);
		$this->method and $this->options[CURLOPT_CUSTOMREQUEST] = $this->method;

		if ( ! empty($this->method))
		{
			$this->options[CURLOPT_CUSTOMREQUEST] = $this->method;
			$this->{'method_'.strtolower($this->method)}();
		}
		else
		{
			$this->method_get();
		}

		$connection = $this->connection();

		curl_setopt_array($connection, $this->options);

		// Execute the request & and hide all output
		$body = curl_exec($connection);
		$this->response_info = curl_getinfo($connection);
		$mime = isset($this->headers['Accept']) ? $this->headers['Accept'] : $this->response_info('content_type', 'text/plain');

		// Was header data requested?
		$headers = array();
		if (isset($this->options[CURLOPT_HEADER]) and $this->options[CURLOPT_HEADER])
		{
			// Split the headers from the body
			$raw_headers = explode("\n", str_replace("\r", "", substr($body, 0, $this->response_info['header_size'])));
			$body = $this->response_info['header_size'] >= strlen($body) ? '' : substr($body, $this->response_info['header_size']);

			// Convert the header data
			foreach ($raw_headers as $header)
			{
				$header = explode(':', $header, 2);
				if (isset($header[1]))
				{
					$headers[trim($header[0])] = trim($header[1]);
				}
			}
		}

		$this->set_response($body, $this->response_info('http_code', 200), $mime, $headers);

		// Request failed
		if ($body === false)
		{
			$this->set_defaults();
			throw new \RequestException(curl_error($connection), curl_errno($connection));
		}
		elseif ($this->response->status >= 400)
		{
			$this->set_defaults();
			throw new \RequestStatusException($body, $this->response->status);
		}
		else
		{
			// Request successful
			curl_close($connection);
			$this->set_defaults();

			return $this;
		}
	}

	/**
	 * Extends parent to reset headers as well
	 *
	 * @return  Request_Curl
	 */
	protected function set_defaults()
	{
		parent::set_defaults();
		$this->headers = array();

		if ( ! empty($this->preserve_resource))
		{
			$this->resource = $this->preserve_resource;
			$this->preserve_resource = null;
		}

		return $this;
	}

	/**
	 * GET request
	 *
	 * @param   array  $params
	 * @param   array  $options
	 * @return  void
	 */
	protected function method_get()
	{
		$this->preserve_resource = $this->resource;
		$this->resource = \Uri::create($this->resource, array(), $this->params);
	}

	/**
	 * POST request
	 *
	 * @param   array  $params
	 * @return  void
	 */
	protected function method_post()
	{
		$params = is_array($this->params) ? $this->encode($this->params) : $this->params;

		$this->set_option(CURLOPT_POST, true);
		$this->set_option(CURLOPT_POSTFIELDS, $params);
	}

	/**
	 * PUT request
	 *
	 * @param   array  $params
	 * @return  void
	 */
	protected function method_put()
	{
		$params = is_array($this->params) ? $this->encode($this->params) : $this->params;

		$this->set_option(CURLOPT_POSTFIELDS, $params);

		// Override method, I think this makes $_POST DELETE data but... we'll see eh?
		$this->set_header('X-HTTP-Method-Override', 'PUT');
	}

	/**
	 * DELETE request
	 *
	 * @param   array  $params
	 * @return  void
	 */
	protected function method_delete()
	{
		$params = is_array($this->params) ? $this->encode($this->params) : $this->params;

		$this->set_option(CURLOPT_POSTFIELDS, $params);

		// Override method, I think this makes $_POST DELETE data but... we'll see eh?
		$this->set_header('X-HTTP-Method-Override', 'DELETE');
	}

	/**
	 * Function to encode input array depending on the content type
	 *
	 * @param   array $input
	 * @return  mixed encoded output
	 */
	protected function encode(array $input)
	{
		// Detect the request content type, default to 'text/plain'
		$content_type = isset($this->headers['Content-Type']) ? $this->headers['Content-Type'] : $this->response_info('content_type', 'text/plain');

		// Get the correct format for the current content type
		$format = \Arr::key_exists(static::$auto_detect_formats, $content_type) ? static::$auto_detect_formats[$content_type] : null;

		switch($format)
		{
			// Format as XML
			case 'xml':
					/**
					 * If the input array has one item in the top level
					 * then use that item as the root XML element.
					 */
					if(count($input) === 1)
					{
						$base_node = key($input);
						return \Format::forge($input[$base_node])->to_xml(null, null, $base_node);
					}
					else
					{
						return \Format::forge($input)->to_xml();
					}
				break;

			// Format as JSON
			case 'json':
					return \Format::forge($input)->to_json();
				break;

			// Format as PHP Serialized Array
			case 'serialize':
					return \Format::forge($input)->to_serialize();
				break;

			// Format as PHP Array
			case 'php':
					return \Format::forge($input)->to_php();
				break;

			// Format as CSV
			case 'csv':
					return \Format::forge($input)->to_csv();
				break;

			// Format as Query String
			default:
					return http_build_query($input, null, '&');
				break;
		}
	}
}
