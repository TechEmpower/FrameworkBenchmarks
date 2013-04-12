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
 * Security Class
 *
 * @package		Fuel
 * @category	Core
 * @author		Dan Horrigan
 * @link		http://docs.fuelphp.com/classes/security.html
 */
class Security
{

	/**
	 * @var  string  the token as submitted in the cookie from the previous request
	 */
	protected static $csrf_old_token = false;

	/**
	 * @var  string  the array key for cookie & post vars to check for the token
	 */
	protected static $csrf_token_key = false;

	/**
	 * @var  string  the token for the next request
	 */
	protected static $csrf_token = false;

	/**
	 * Class init
	 *
	 * Fetches CSRF settings and current token
	 */
	public static function _init()
	{
		static::$csrf_token_key = \Config::get('security.csrf_token_key', 'fuel_csrf_token');
		static::$csrf_old_token = \Input::cookie(static::$csrf_token_key, false);

		// if csrf automatic checking is enabled, and it fails validation, bail out!
		if (\Config::get('security.csrf_autoload', true))
		{
			static::check_token();
		}

		// throw an exception if the output filter setting is missing from the app config
		if (\Config::get('security.output_filter', null) === null)
		{
			throw new \FuelException('There is no security.output_filter defined in your application config file');
		}

		// deal with duplicate filters, no need to slow the framework down
		foreach (array('output_filter', 'uri_filter', 'input_filter') as $setting)
		{
			$config = \Config::get('security.'.$setting, array());
			is_array($config) and \Config::set('security.'.$setting, array_keys(array_flip($config)));
		}
	}

	/**
	 * Cleans the request URI
	 *
	 * @param  string  $uri     uri to clean
	 * @param  bool    $strict  whether to remove relative directories
	 */
	public static function clean_uri($uri, $strict = false)
	{
		$filters = \Config::get('security.uri_filter', array());
		$filters = is_array($filters) ? $filters : array($filters);

		$strict and $uri = preg_replace(array("/\.+\//", '/\/+/'), '/', $uri);

		return static::clean($uri, $filters);
	}

	/**
	 * Cleans the global $_GET, $_POST and $_COOKIE arrays
	 */
	public static function clean_input()
	{
		$_GET		= static::clean($_GET);
		$_POST		= static::clean($_POST);
		$_COOKIE	= static::clean($_COOKIE);
	}

	/**
	 * Generic variable clean method
	 */
	public static function clean($var, $filters = null, $type = 'security.input_filter')
	{
		is_null($filters) and $filters = \Config::get($type, array());
		$filters = is_array($filters) ? $filters : array($filters);

		foreach ($filters as $filter)
		{
			// is this filter a callable local function?
			if (is_string($filter) and is_callable('static::'.$filter))
			{
				$var = static::$filter($var);
			}

			// is this filter a callable function?
			elseif (is_callable($filter))
			{
				if (is_array($var))
				{
					foreach($var as $key => $value)
					{
						$var[$key] = call_user_func($filter, $value);
					}
				}
				else
				{
					$var = call_user_func($filter, $var);
				}
			}

			// assume it's a regex of characters to filter
			else
			{
				if (is_array($var))
				{
					foreach($var as $key => $value)
					{
						$var[$key] = preg_replace('#['.$filter.']#ui', '', $value);
					}
				}
				else
				{
					$var = preg_replace('#['.$filter.']#ui', '', $var);
				}
			}
		}
		return $var;
	}

	public static function xss_clean($value)
	{
		if ( ! is_array($value))
		{
			if ( ! function_exists('htmLawed'))
			{
				import('htmlawed/htmlawed', 'vendor');
			}

			return htmLawed($value, array('safe' => 1, 'balanced' => 0));
		}

		foreach ($value as $k => $v)
		{
			$value[$k] = static::xss_clean($v);
		}

		return $value;
	}

	public static function strip_tags($value)
	{
		if ( ! is_array($value))
		{
			$value = filter_var($value, FILTER_SANITIZE_STRING);
		}
		else
		{
			foreach ($value as $k => $v)
			{
				$value[$k] = static::strip_tags($v);
			}
		}

		return $value;
	}

	public static function htmlentities($value, $flags = null, $encoding = null, $double_encode = null)
	{
		static $already_cleaned = array();

		is_null($flags) and $flags = \Config::get('security.htmlentities_flags', ENT_QUOTES);
		is_null($encoding) and $encoding = \Fuel::$encoding;
		is_null($double_encode) and $double_encode = \Config::get('security.htmlentities_double_encode', false);

		// Nothing to escape for non-string scalars, or for already processed values
		if (is_bool($value) or is_int($value) or is_float($value) or in_array($value, $already_cleaned, true))
		{
			return $value;
		}

		if (is_string($value))
		{
			$value = htmlentities($value, $flags, $encoding, $double_encode);
		}
		elseif (is_array($value) or ($value instanceof \Iterator and $value instanceof \ArrayAccess))
		{
			// Add to $already_cleaned variable when object
			is_object($value) and $already_cleaned[] = $value;

			foreach ($value as $k => $v)
			{
				$value[$k] = static::htmlentities($v, $flags, $encoding, $double_encode);
			}
		}
		elseif ($value instanceof \Iterator or get_class($value) == 'stdClass')
		{
			// Add to $already_cleaned variable
			$already_cleaned[] = $value;

			foreach ($value as $k => $v)
			{
				$value->{$k} = static::htmlentities($v, $flags, $encoding, $double_encode);
			}
		}
		elseif (is_object($value))
		{
			// Check if the object is whitelisted and return when that's the case
			foreach (\Config::get('security.whitelisted_classes', array()) as $class)
			{
				if (is_a($value, $class))
				{
					// Add to $already_cleaned variable
					$already_cleaned[] = $value;

					return $value;
				}
			}

			// Throw exception when it wasn't whitelisted and can't be converted to String
			if ( ! method_exists($value, '__toString'))
			{
				throw new \RuntimeException('Object class "'.get_class($value).'" could not be converted to string or '.
					'sanitized as ArrayAccess. Whitelist it in security.whitelisted_classes in app/config/config.php '.
					'to allow it to be passed unchecked.');
			}

			$value = static::htmlentities((string) $value, $flags, $encoding, $double_encode);
		}

		return $value;
	}

	/**
	 * Check CSRF Token
	 *
	 * @param   string  CSRF token to be checked, checks post when empty
	 * @return  bool
	 */
	public static function check_token($value = null)
	{
		$value = $value ?: \Input::post(static::$csrf_token_key, 'fail');

		// always reset token once it's been checked and still the same
		if (static::fetch_token() == static::$csrf_old_token and ! empty($value))
		{
			static::set_token(true);
		}

		return $value === static::$csrf_old_token;
	}

	/**
	 * Fetch CSRF Token for the next request
	 *
	 * @return  string
	 */
	public static function fetch_token()
	{
		if (static::$csrf_token !== false)
		{
			return static::$csrf_token;
		}

		static::set_token();

		return static::$csrf_token;
	}

	protected static function set_token($reset = false)
	{
		// re-use old token when found (= not expired) and expiration is used (otherwise always reset)
		if ( ! $reset and static::$csrf_old_token and \Config::get('security.csrf_expiration', 0) > 0)
		{
			static::$csrf_token = static::$csrf_old_token;
		}
		// set new token for next session when necessary
		else
		{
			static::$csrf_token = md5(uniqid().time());

			$expiration = \Config::get('security.csrf_expiration', 0);
			\Cookie::set(static::$csrf_token_key, static::$csrf_token, $expiration);
		}
	}

	/**
	 * JS fetch token
	 *
	 * Produces JavaScript fuel_csrf_token() function that will return the current
	 * CSRF token when called. Use to fill right field on form submit for AJAX operations.
	 *
	 * @return string
	 */
	public static function js_fetch_token()
	{
		$output  = '<script type="text/javascript">
	function fuel_csrf_token()
	{
		if (document.cookie.length > 0)
		{
			var c_name = "'.static::$csrf_token_key.'";
			c_start = document.cookie.indexOf(c_name + "=");
			if (c_start != -1)
			{
				c_start = c_start + c_name.length + 1;
				c_end = document.cookie.indexOf(";" , c_start);
				if (c_end == -1)
				{
					c_end=document.cookie.length;
				}
				return unescape(document.cookie.substring(c_start, c_end));
			}
		}
		return "";
	}'.PHP_EOL;
		$output .= '</script>'.PHP_EOL;

		return $output;
	}

	/**
	 * JS set token
	 *
	 * Produces JavaScript fuel_set_csrf_token() function that will update the current
	 * CSRF token in the form when called, based on the value of the csrf cookie
	 *
	 * @return string
	 */
	public static function js_set_token()
	{
		$output  = '<script type="text/javascript">
	function fuel_set_csrf_token(form)
	{
		if (document.cookie.length > 0 && typeof form != undefined)
		{
			var c_name = "'.static::$csrf_token_key.'";
			c_start = document.cookie.indexOf(c_name + "=");
			if (c_start != -1)
			{
				c_start = c_start + c_name.length + 1;
				c_end = document.cookie.indexOf(";" , c_start);
				if (c_end == -1)
				{
					c_end=document.cookie.length;
				}
				value=unescape(document.cookie.substring(c_start, c_end));
				if (value != "")
				{
					for(i=0; i<form.elements.length; i++)
					{
						if (form.elements[i].name == c_name)
						{
							form.elements[i].value = value;
							break;
						}
					}
				}
			}
		}
	}'.PHP_EOL;
		$output .= '</script>'.PHP_EOL;

		return $output;
	}
}
