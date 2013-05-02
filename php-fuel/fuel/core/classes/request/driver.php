<?php

namespace Fuel\Core;

class RequestException extends \HttpNotFoundException {}
class RequestStatusException extends \RequestException {}

abstract class Request_Driver
{
	/**
	 * Forge
	 *
	 * @param   string  $resource
	 * @param   array   $options
	 * @return  Request_Driver
	 */
	public static function forge($resource, array $options = array(), $method = null)
	{
		return new static($resource, $options, $method);
	}

	/**
	 * @var  string  URL resource to perform requests upon
	 */
	protected $resource = '';

	/**
	 * @var  array  parameters to pass
	 */
	protected $params = array();

	/**
	 * @var  array  params set during object creation are handled as the defaults
	 */
	protected $default_params = array();

	/**
	 * @var  array  driver specific options
	 */
	protected $options = array();

	/**
	 * @var  array  options set during object creation are handled as the defaults
	 */
	protected $default_options = array();

	/**
	 * @var  array  http headers set for the request
	 */
	protected $headers = array();

	/**
	 * @var  Response  the response object after execute
	 */
	protected $response;

	/**
	 * @var  array  info about the response
	 */
	protected $response_info = array();

	/**
	 * @var  bool  whether to attempt auto-formatting the response
	 */
	protected $auto_format = true;

	/**
	 * @var  string  $method  request method
	 */
	protected $method = null;

	/**
	 * @var  array  supported response formats
	 */
	protected static $supported_formats = array(
		'xml' => 'application/xml',
		'json' => 'application/json',
		'serialize' => 'application/vnd.php.serialized',
		'php' => 'text/plain',
		'csv' => 'text/csv',
	);

	/**
	 * @var  array  mimetype format autodetection
	 */
	protected static $auto_detect_formats = array(
		'application/xml' => 'xml',
		'text/xml' => 'xml',
		'application/json' => 'json',
		'text/json' => 'json',
		'text/csv' => 'csv',
		'application/csv' => 'csv',
		'application/vnd.php.serialized' => 'serialize',
	);

	public function __construct($resource, array $options, $method = null)
	{
		$this->resource  = $resource;
		$method and $this->set_method($method);

		foreach ($options as $key => $value)
		{
			if (method_exists($this, 'set_'.$key))
			{
				$this->{'set_'.$key}($value);
			}
		}

		$this->default_options  = $this->options;
		$this->default_params   = $this->params;
	}

	/**
	 * Sets the request method.
	 *
	 * @param   string  $method  request method
	 * @return  object  current instance
	 */
	public function set_method($method)
	{
		$this->method = strtoupper($method);
		return $this;
	}

	/**
	 * Returns the request method.
	 *
	 * @return  string  request method
	 */
	public function get_method()
	{
		return $this->method;
	}

	/**
	 * Set the parameters to pass with the request
	 *
	 * @param   array  $params
	 * @return  Request_Driver
	 */
	public function set_params($params)
	{
		$this->params = $params;
		return $this;
	}

	/**
	 * Sets options on the driver
	 *
	 * @param   array  $options
	 * @return  Request_Driver
	 */
	public function set_options(array $options)
	{
		foreach ($options as $key => $val)
		{
			$this->options[$key] = $val;
		}

		return $this;
	}

	/**
	 * Sets a single option/value
	 *
	 * @param   int|string  $option
	 * @param   mixed       $value
	 * @return  Request_Driver
	 */
	public function set_option($option, $value)
	{
		return $this->set_options(array($option => $value));
	}

	/**
	 * Add a single parameter/value or an array of parameters
	 *
	 * @param   string|array  $param
	 * @param   mixed         $value
	 * @return  Request_Driver
	 */
	public function add_param($param, $value = null)
	{
		if ( ! is_array($param))
		{
			$param = array($param, $value);
		}

		foreach ($param as $key => $val)
		{
			\Arr::set($this->params, $key, $val);
		}
		return $this;
	}

	/**
	 * set a request http header
	 *
	 * @param   string  $header
	 * @param   string  $header
	 * @return  Request_Driver
	 */
	public function set_header($header, $content = null)
	{
		if (is_null($content))
		{
			$this->headers[] = $header;
		}
		else
		{
			$this->headers[$header] = $content;
		}

		return $this;
	}

	/**
	 * Collect all headers and parse into consistent string
	 *
	 * @return  array
	 */
	public function get_headers()
	{
		$headers = array();
		foreach ($this->headers as $key => $value)
		{
			$headers[] = is_int($key) ? $value : $key.': '.$value;
		}

		return $headers;
	}

	/**
	 * Set mime-type accept header
	 *
	 * @param   string  $mime
	 * @return  string  Request_Driver
	 */
	public function set_mime_type($mime)
	{
		if (array_key_exists($mime, static::$supported_formats))
		{
			$mime = static::$supported_formats[$mime];
		}

		$this->set_header('Accept', $mime);
		return $this;
	}

	/**
	 * Switch auto formatting on or off
	 *
	 * @param   bool  $auto_format
	 * @return  Request_Driver
	 */
	public function set_auto_format($auto_format)
	{
		$this->auto_format = (bool) $auto_format;
		return $this;
	}

	/**
	 * Executes the request upon the URL
	 *
	 * @param   array  $additional_params
	 * @param   array  $query_string
	 * @return  Response
	 */
	abstract public function execute(array $additional_params = array());

	/**
	 * Reset before doing another request
	 *
	 * @return  Request_Driver
	 */
	protected function set_defaults()
	{
		$this->options   = $this->default_options;
		$this->params    = $this->default_params;
		return $this;
	}

	/**
	 * Creates the Response and optionally attempts to auto-format the output
	 *
	 * @param   string  $body
	 * @param   int     $status
	 * @param   string  $mime
	 * @param   array   $headers
	 * @return  Response
	 */
	public function set_response($body, $status, $mime = null, $headers = array())
	{
		if ($this->auto_format and array_key_exists($mime, static::$auto_detect_formats))
		{
			$body = \Format::forge($body, static::$auto_detect_formats[$mime])->to_array();
		}

		$this->response = \Response::forge($body, $status, $headers);
		return $this->response;
	}

	/**
	 * Fetch the response
	 *
	 * @return  Response
	 */
	public function response()
	{
		return $this->response;
	}

	/**
	 * Fetch the response info or a key from it
	 *
	 * @param   string  $key
	 * @param   string  $default
	 * @return  mixed
	 */
	public function response_info($key = null, $default = null)
	{
		if (func_num_args() == 0)
		{
			return $this->response_info;
		}

		return \Arr::get($this->response_info, $key, $default);
	}

	/**
	 * Returns the body as a string.
	 *
	 * @return  string
	 */
	public function __toString()
	{
		return (string) $this->response();
	}
}
