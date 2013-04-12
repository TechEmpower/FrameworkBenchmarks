<?php

namespace Fuel\Core;

class Request_Soap extends \Request_Driver
{
	protected static $wsdl_settings = array('wsdl', 'classmap', 'cache_wsdl');
	protected static $non_wsdl_settings = array('location', 'uri', 'style', 'use');
	protected static $generic_settings = array(
		'soap_version', 'compression', 'encoding', 'trace', 'connection_timeout',
		'typemap', 'user_agent', 'stream_context', 'features',
	);

	/**
	 * @var  \SoapClient	holds the SoapClient object used for the connection
	 */
	protected $connection;

	/**
	 * @var  string  function to call
	 */
	protected $function = '';

	/**
	 * Extends parent constructor to detect availability of cURL
	 *
	 * @param   string  $resource
	 * @param   array   $options
	 * @throws  \RuntimeException
	 */
	public function __construct($resource, array $options)
	{
		// check if we have libcurl available
		if ( ! class_exists('SoapClient'))
		{
			throw new \RuntimeException('Your PHP installation doesn\'t have Soap enabled. Rebuild PHP with --enable-soap');
		}

		logger(\Fuel::L_INFO, 'Creating a new SOAP Request with URI = "'.$resource.'"', __METHOD__);

		// If authentication is enabled use it
		if ( ! empty($options['user']) and ! empty($options['pass']))
		{
			$this->set_option('login', $options['user']);
			$this->set_option('password', $options['pass']);
		}

		// WSDL-mode only options
		if ( ! empty($resource))
		{
			foreach (static::$wsdl_settings as $setting)
			{
				isset($options[$setting]) and $this->set_option($setting, $options[$setting]);
			}
		}
		// non-WSDL-mode only options
		else
		{
			$resource = null;

			if ( ! isset($options['location']) or ! isset($options['uri']))
			{
				throw new \RequestException('The keys "location" and "uri" are required in non-WSDL mode.');
			}

			foreach (static::$non_wsdl_settings as $setting)
			{
				isset($options[$setting]) and $this->set_option($setting, $options[$setting]);
			}
		}

		foreach (static::$generic_settings as $setting)
		{
			isset($options[$setting]) and $this->set_option($setting, $options[$setting]);
		}

		// make it always throw exceptions
		$this->set_option('exceptions', true);

		parent::__construct($resource, $options);
	}

	/**
	 * Set the function to execute on the SoapClient
	 *
	 * @param   string  $function
	 * @return  Request_Soap
	 */
	public function set_function($function)
	{
		$this->function = $function;
		return $this;
	}

	/**
	 * Fetch the connection, create if necessary
	 *
	 * @return  \SoapClient
	 */
	protected function connection()
	{
		if (empty($this->connection))
		{
			$this->connection = new \SoapClient($this->resource, $this->options);
		}

		return $this->connection;
	}

	public function execute(array $additional_params = array())
	{
		if (empty($this->function))
		{
			throw new \RequestException('No function set to execute on the Soap request.');
		}

		$additional_params and $this->params = \Arr::merge($this->params, $additional_params);

		// Execute the request & and hide all output
		try
		{
			$body = $this->connection()->__soapCall($this->function, $this->params, array(), $this->get_headers(), $headers);
			$this->response_info = $headers;
			$mime = isset($this->headers['Accept']) ? $this->headers['Accept'] : null;
			$this->set_response($body, $this->response_info('http_code', 200), $mime, $headers);

			$this->set_defaults();
			return $this;
		}
		catch (\SoapFault $e)
		{
			$this->set_defaults();
			throw new \RequestException($e->getMessage(), $e->getCode(), $e);
		}
	}

	/**
	 * Extends parent to reset headers as well
	 *
	 * @return  Request_Soap
	 */
	protected function set_defaults()
	{
		parent::set_defaults();
		$this->function = '';

		return $this;
	}

	/**
	 * Get functions defined in WSDL
	 *
	 * @return  array
	 * @throws  \RequestException
	 */
	public function get_functions()
	{
		if ( ! $this->resource)
		{
			throw new \RequestException('SOAP get functions not available in non-WSDL mode.');
		}
		return $this->connection()->__getFunctions();
	}

	/**
	 * Get last request XML
	 *
	 * @return  string
	 * @throws  \RequestException
	 */
	public function get_request_xml()
	{
		if (empty($this->options['trace']))
		{
			throw new \RequestException('The "trace" option must be true to be able to get the last request.');
		}
		return $this->connection()->__getLastRequest();
	}

	/**
	 * Get last request headers
	 *
	 * @return  string
	 * @throws  \RequestException
	 */
	public function get_request_headers()
	{
		if (empty($this->options['trace']))
		{
			throw new \RequestException('The "trace" option must be true to be able to get the last request headers.');
		}
		return $this->connection()->__getLastRequestHeaders();
	}

	/**
	 * Get last response XML
	 *
	 * @return  string
	 * @throws  \RequestException
	 */
	public function get_response_xml()
	{
		if (empty($this->options['trace']))
		{
			throw new \RequestException('The "trace" option must be true to be able to get the last response.');
		}
		return $this->connection()->__getLastResponse();
	}

	/**
	 * Get last response headers
	 *
	 * @return  string
	 * @throws  \RequestException
	 */
	public function get_response_headers()
	{
		if (empty($this->options['trace']))
		{
			throw new \RequestException('The "trace" option must be true to be able to get the last response headers.');
		}
		return $this->connection()->__getLastResponseHeaders();
	}

	/**
	 * Get last response headers
	 *
	 * @return  array
	 * @throws  \RequestException
	 */
	public function get_types()
	{
		if ( ! $this->resource)
		{
			throw new \RequestException('SOAP get types not available in non-WSDL mode.');
		}
		return $this->connection()->__getTypes();
	}

	/**
	 * Set cookie for subsequent requests
	 *
	 * @param   string  $name
	 * @param   string  $value
	 * @return  void
	 * @throws  \RequestException
	 */
	public function set_cookie($name, $value = null)
	{
		is_null($value)
			? $this->connection()->__setCookie($name)
			: $this->connection()->__setCookie($name, $value);
	}

	/**
	 * Change the endpoint location
	 *
	 * @param   string  $location
	 * @return  string  the old endpoint
	 */
	public function set_location($location)
	{
		$this->connection()->__setLocation($location);
	}
}
