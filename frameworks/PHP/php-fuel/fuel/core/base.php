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

/**
 * Loads in a core class and optionally an app class override if it exists.
 *
 * @param   string  $path
 * @param   string  $folder
 * @return  void
 */
if ( ! function_exists('import'))
{
	function import($path, $folder = 'classes')
	{
		$path = str_replace('/', DIRECTORY_SEPARATOR, $path);
		require_once COREPATH.$folder.DIRECTORY_SEPARATOR.$path.'.php';

		if (is_file(APPPATH.$folder.DIRECTORY_SEPARATOR.$path.'.php'))
		{
			require_once APPPATH.$folder.DIRECTORY_SEPARATOR.$path.'.php';
		}
	}
}


if ( ! function_exists('logger'))
{
	function logger($level, $msg, $method = null)
	{
		static $labels = array(
			100 => 'DEBUG',
			200 => 'INFO',
			250 => 'NOTICE',
			300 => 'WARNING',
			400 => 'ERROR',
			500 => 'CRITICAL',
			550 => 'ALERT',
			600 => 'EMERGENCY',
			700 => 'ALL',
		);

		// make sure $level has the correct value
		if ((is_int($level) and ! isset($labels[$level])) or (is_string($level) and ! array_search(strtoupper($level), $labels)))
		{
			throw new \FuelException('Invalid level "'.$level.'" passed to logger()');
		}

		// get the levels defined to be logged
		$loglabels = \Config::get('log_threshold');

		// bail out if we don't need logging at all
		if ($loglabels == \Fuel::L_NONE)
		{
			return false;
		}

		// if profiling is active log the message to the profile
		if (\Config::get('profiling'))
		{
			\Console::log($method.' - '.$msg);
		}

		// if it's not an array, assume it's an "up to" level
		if ( ! is_array($loglabels))
		{
			$a = array();
			foreach ($labels as $l => $label)
			{
				$l >= $loglabels and $a[] = $l;
			}
			$loglabels = $a;
		}

		// do we need to log the message with this level?
		if ( ! in_array($level, $loglabels))
		{
			return false;
		}

		! class_exists('Log') and \Package::load('log');

		return \Log::instance()->log($level, (empty($method) ? '' : $method.' - ').$msg);
	}
}


/**
 * Takes an array of attributes and turns it into a string for an html tag
 *
 * @param	array	$attr
 * @return	string
 */
if ( ! function_exists('array_to_attr'))
{
	function array_to_attr($attr)
	{
		$attr_str = '';

		foreach ((array) $attr as $property => $value)
		{
			// Ignore null/false
			if ($value === null or $value === false)
			{
				continue;
			}

			// If the key is numeric then it must be something like selected="selected"
			if (is_numeric($property))
			{
				$property = $value;
			}

			$attr_str .= $property.'="'.$value.'" ';
		}

		// We strip off the last space for return
		return trim($attr_str);
	}
}

/**
 * Create a XHTML tag
 *
 * @param	string			The tag name
 * @param	array|string	The tag attributes
 * @param	string|bool		The content to place in the tag, or false for no closing tag
 * @return	string
 */
if ( ! function_exists('html_tag'))
{
	function html_tag($tag, $attr = array(), $content = false)
	{
		$has_content = (bool) ($content !== false and $content !== null);
		$html = '<'.$tag;

		$html .= ( ! empty($attr)) ? ' '.(is_array($attr) ? array_to_attr($attr) : $attr) : '';
		$html .= $has_content ? '>' : ' />';
		$html .= $has_content ? $content.'</'.$tag.'>' : '';

		return $html;
	}
}

/**
 * A case-insensitive version of in_array.
 *
 * @param	mixed	$needle
 * @param	array	$haystack
 * @return	bool
 */
if ( ! function_exists('in_arrayi'))
{
	function in_arrayi($needle, $haystack)
	{
		return in_array(strtolower($needle), array_map('strtolower', $haystack));
	}
}

/**
 * Gets all the public vars for an object.  Use this if you need to get all the
 * public vars of $this inside an object.
 *
 * @return	array
 */
if ( ! function_exists('get_object_public_vars'))
{
	function get_object_public_vars($obj)
	{
		return get_object_vars($obj);
	}
}

/**
 * Renders a view and returns the output.
 *
 * @param   string	The view name/path
 * @param   array	The data for the view
 * @param   bool    Auto filter override
 * @return  string
 */
if ( ! function_exists('render'))
{
	function render($view, $data = null, $auto_filter = null)
	{
		return \View::forge($view, $data, $auto_filter)->render();
	}
}

/**
 * A wrapper function for Lang::get()
 *
 * @param	mixed	The string to translate
 * @param	array	The parameters
 * @return	string
 */
if ( ! function_exists('__'))
{
	function __($string, $params = array(), $default = null)
	{
		return \Lang::get($string, $params, $default);
	}
}

/**
 * Encodes the given string.  This is just a wrapper function for Security::htmlentities()
 *
 * @param	mixed	The string to encode
 * @return	string
 */
if ( ! function_exists('e'))
{
	function e($string)
	{
		return Security::htmlentities($string);
	}
}

/**
 * Takes a classname and returns the actual classname for an alias or just the classname
 * if it's a normal class.
 *
 * @param   string  classname to check
 * @return  string  real classname
 */
if ( ! function_exists('get_real_class'))
{
	function get_real_class($class)
	{
		static $classes = array();

		if ( ! array_key_exists($class, $classes))
		{
			$reflect = new ReflectionClass($class);
			$classes[$class] = $reflect->getName();
		}

		return $classes[$class];
	}
}

/**
 * Takes an associative array in the layout of parse_url, and constructs a URL from it
 *
 * see http://www.php.net/manual/en/function.http-build-url.php#96335
 *
 * @param   mixed   (Part(s) of) an URL in form of a string or associative array like parse_url() returns
 * @param   mixed   Same as the first argument
 * @param   int     A bitmask of binary or'ed HTTP_URL constants (Optional)HTTP_URL_REPLACE is the default
 * @param   array   If set, it will be filled with the parts of the composed url like parse_url() would return
 *
 * @return  string  constructed URL
 */
if (!function_exists('http_build_url'))
{
	define('HTTP_URL_REPLACE', 1);				// Replace every part of the first URL when there's one of the second URL
	define('HTTP_URL_JOIN_PATH', 2);			// Join relative paths
	define('HTTP_URL_JOIN_QUERY', 4);			// Join query strings
	define('HTTP_URL_STRIP_USER', 8);			// Strip any user authentication information
	define('HTTP_URL_STRIP_PASS', 16);			// Strip any password authentication information
	define('HTTP_URL_STRIP_AUTH', 32);			// Strip any authentication information
	define('HTTP_URL_STRIP_PORT', 64);			// Strip explicit port numbers
	define('HTTP_URL_STRIP_PATH', 128);			// Strip complete path
	define('HTTP_URL_STRIP_QUERY', 256);		// Strip query string
	define('HTTP_URL_STRIP_FRAGMENT', 512);		// Strip any fragments (#identifier)
	define('HTTP_URL_STRIP_ALL', 1024);			// Strip anything but scheme and host

	function http_build_url($url, $parts = array(), $flags = HTTP_URL_REPLACE, &$new_url = false)
	{
		$keys = array('user','pass','port','path','query','fragment');

		// HTTP_URL_STRIP_ALL becomes all the HTTP_URL_STRIP_Xs
		if ($flags & HTTP_URL_STRIP_ALL)
		{
			$flags |= HTTP_URL_STRIP_USER;
			$flags |= HTTP_URL_STRIP_PASS;
			$flags |= HTTP_URL_STRIP_PORT;
			$flags |= HTTP_URL_STRIP_PATH;
			$flags |= HTTP_URL_STRIP_QUERY;
			$flags |= HTTP_URL_STRIP_FRAGMENT;
		}
		// HTTP_URL_STRIP_AUTH becomes HTTP_URL_STRIP_USER and HTTP_URL_STRIP_PASS
		else if ($flags & HTTP_URL_STRIP_AUTH)
		{
			$flags |= HTTP_URL_STRIP_USER;
			$flags |= HTTP_URL_STRIP_PASS;
		}

		// parse the original URL
		$parse_url = is_array($url) ? $url : parse_url($url);

		// make sure we always have a scheme, host and path
		empty($parse_url['scheme']) and $parse_url['scheme'] = 'http';
		empty($parse_url['host']) and $parse_url['host'] = \Input::server('http_host');
		isset($parse_url['path']) or $parse_url['path'] = '';

		// make the path absolute if needed
		if ( ! empty($parse_url['path']) and substr($parse_url['path'], 0, 1) != '/')
		{
			$parse_url['path'] = '/'.$parse_url['path'];
		}

		// scheme and host are always replaced
		isset($parts['scheme']) and $parse_url['scheme'] = $parts['scheme'];
		isset($parts['host']) and $parse_url['host'] = $parts['host'];

		// replace the original URL with it's new parts (if applicable)
		if ($flags & HTTP_URL_REPLACE)
		{
			foreach ($keys as $key)
			{
				if (isset($parts[$key]))
					$parse_url[$key] = $parts[$key];
			}
		}
		else
		{
			// join the original URL path with the new path
			if (isset($parts['path']) && ($flags & HTTP_URL_JOIN_PATH))
			{
				if (isset($parse_url['path']))
					$parse_url['path'] = rtrim(str_replace(basename($parse_url['path']), '', $parse_url['path']), '/') . '/' . ltrim($parts['path'], '/');
				else
					$parse_url['path'] = $parts['path'];
			}

			// join the original query string with the new query string
			if (isset($parts['query']) && ($flags & HTTP_URL_JOIN_QUERY))
			{
				if (isset($parse_url['query']))
					$parse_url['query'] .= '&' . $parts['query'];
				else
					$parse_url['query'] = $parts['query'];
			}
		}

		// strips all the applicable sections of the URL
		// note: scheme and host are never stripped
		foreach ($keys as $key)
		{
			if ($flags & (int)constant('HTTP_URL_STRIP_' . strtoupper($key)))
				unset($parse_url[$key]);
		}


		$new_url = $parse_url;

		return
			 ((isset($parse_url['scheme'])) ? $parse_url['scheme'] . '://' : '')
			.((isset($parse_url['user'])) ? $parse_url['user'] . ((isset($parse_url['pass'])) ? ':' . $parse_url['pass'] : '') .'@' : '')
			.((isset($parse_url['host'])) ? $parse_url['host'] : '')
			.((isset($parse_url['port'])) ? ':' . $parse_url['port'] : '')
			.((isset($parse_url['path'])) ? $parse_url['path'] : '')
			.((isset($parse_url['query'])) ? '?' . $parse_url['query'] : '')
			.((isset($parse_url['fragment'])) ? '#' . $parse_url['fragment'] : '')
		;
	}
}
