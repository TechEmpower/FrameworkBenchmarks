<?php
/**
 * Core Bootstrap
 *
 * This file contains all common system functions and View and Controller classes.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */


/**
 * Attach (or remove) multiple callbacks to an event and trigger those callbacks when that event is called.
 *
 * @param string $event name
 * @param mixed $value the optional value to pass to each callback
 * @param mixed $callback the method or function to call - FALSE to remove all callbacks for event
 */
function event($event, $value = NULL, $callback = NULL)
{
	static $events;

	// Adding or removing a callback?
	if($callback !== NULL)
	{
		if($callback)
		{
			$events[$event][] = $callback;
		}
		else
		{
			unset($events[$event]);
		}
	}
	elseif(isset($events[$event])) // Fire a callback
	{
		foreach($events[$event] as $function)
		{
			$value = call_user_func($function, $value);
		}
		return $value;
	}
}


/**
 * Fetch a config value from a module configuration file
 *
 * @param string $file name of the config
 * @param boolean $clear to clear the config object
 * @return object
 */
function config($file = 'Config', $clear = FALSE)
{
	static $configs = array();

	if($clear)
	{
		unset($configs[$file]);
		return;
	}

	if(empty($configs[$file]))
	{
		//$configs[$file] = new \Micro\Config($file);
		require(SP . 'Config/' . $file . EXT);
		$configs[$file] = (object) $config;
		//print dump($configs);
	}

	return $configs[$file];
}


/**
 * Return an HTML safe dump of the given variable(s) surrounded by "pre" tags.
 * You can pass any number of variables (of any type) to this function.
 *
 * @param mixed
 * @return string
 */
function dump()
{
	$string = '';
	foreach(func_get_args() as $value)
	{
		$string .= '<pre>' . h($value === NULL ? 'NULL' : (is_scalar($value) ? $value : print_r($value, TRUE))) . "</pre>\n";
	}
	return $string;
}


/**
 * Safely fetch a $_POST value, defaulting to the value provided if the key is
 * not found.
 *
 * @param string $key name
 * @param mixed $default value if key is not found
 * @param boolean $string TRUE to require string type
 * @return mixed
 */
function post($key, $default = NULL, $string = FALSE)
{
	if(isset($_POST[$key]))
	{
		return $string ? (string)$_POST[$key] : $_POST[$key];
	}
	return $default;
}


/**
 * Safely fetch a $_GET value, defaulting to the value provided if the key is
 * not found.
 *
 * @param string $key name
 * @param mixed $default value if key is not found
 * @param boolean $string TRUE to require string type
 * @return mixed
 */
function get($key, $default = NULL, $string = FALSE)
{
	if(isset($_GET[$key]))
	{
		return $string ? (string)$_GET[$key] : $_GET[$key];
	}
	return $default;
}


/**
 * Safely fetch a $_SESSION value, defaulting to the value provided if the key is
 * not found.
 *
 * @param string $k the post key
 * @param mixed $d the default value if key is not found
 * @return mixed
 */
function session($k, $d = NULL)
{
	return isset($_SESSION[$k]) ? $_SESSION[$k] : $d;
}


/**
 * Create a random 32 character MD5 token
 *
 * @return string
 */
function token()
{
	return md5(str_shuffle(chr(mt_rand(32, 126)) . uniqid() . microtime(TRUE)));
}


/**
 * Write to the application log file using error_log
 *
 * @param string $message to save
 * @return bool
 */
function log_message($message)
{
	//$path = SP . 'Storage/Log/' . date('Y-m-d') . '.log';
	$path = 'stderr';

	// Append date and IP to log message
	return error_log(date('H:i:s ') . getenv('REMOTE_ADDR') . " $message\n", 3, $path);
}


/**
 * Send a HTTP header redirect using "location" or "refresh".
 *
 * @param string $url the URL string
 * @param int $c the HTTP status code
 * @param string $method either location or redirect
 */
function redirect($url = NULL, $code = 302, $method = 'location')
{
	if(strpos($url, '://') === FALSE)
	{
		$url = site_url($url);
	}

	//print dump($url);

	header($method == 'refresh' ? "Refresh:0;url = $url" : "Location: $url", TRUE, $code);
}


/*
 * Return the full URL to a path on this site or another.
 *
 * @param string $uri may contain another sites TLD
 * @return string
 *
function site_url($uri = NULL)
{
	return (strpos($uri, '://') === FALSE ? \Micro\URL::get() : '') . ltrim($uri, '/');
}
*/

/**
 * Return the full URL to a location on this site
 *
 * @param string $path to use or FALSE for current path
 * @param array $params to append to URL
 * @return string
 */
function site_url($path = NULL, array $params = NULL)
{
	// In PHP 5.4, http_build_query will support RFC 3986
	return DOMAIN . ($path ? '/'. trim($path, '/') : PATH)
		. ($params ? '?'. str_replace('+', '%20', http_build_query($params, TRUE, '&')) : '');
}


/**
 * Return the current URL with path and query params
 *
 * @return string
 *
function current_url()
{
	return DOMAIN . getenv('REQUEST_URI');
}
*/

/**
 * Convert a string from one encoding to another encoding
 * and remove invalid bytes sequences.
 *
 * @param string $string to convert
 * @param string $to encoding you want the string in
 * @param string $from encoding that string is in
 * @return string
 */
function encode($string, $to = 'UTF-8', $from = 'UTF-8')
{
	// ASCII is already valid UTF-8
	if($to == 'UTF-8' AND is_ascii($string))
	{
		return $string;
	}

	// Convert the string
	return @iconv($from, $to . '//TRANSLIT//IGNORE', $string);
}


/**
 * Tests whether a string contains only 7bit ASCII characters.
 *
 * @param string $string to check
 * @return bool
 */
function is_ascii($string)
{
	return ! preg_match('/[^\x00-\x7F]/S', $string);
}


/**
 * Encode a string so it is safe to pass through the URL
 *
 * @param string $string to encode
 * @return string
 */
function base64_url_encode($string = NULL)
{
	return strtr(base64_encode($string), '+/=', '-_~');
}


/**
 * Decode a string passed through the URL
 *
 * @param string $string to decode
 * @return string
 */
function base64_url_decode($string = NULL)
{
	return base64_decode(strtr($string, '-_~', '+/='));
}


/**
 * Convert special characters to HTML safe entities.
 *
 * @param string $string to encode
 * @return string
 */
function h($string)
{
	return htmlspecialchars($string, ENT_QUOTES, 'utf-8');
}


/**
 * Filter a valid UTF-8 string so that it contains only words, numbers,
 * dashes, underscores, periods, and spaces - all of which are safe
 * characters to use in file names, URI, XML, JSON, and (X)HTML.
 *
 * @param string $string to clean
 * @param bool $spaces TRUE to allow spaces
 * @return string
 */
function sanitize($string, $spaces = TRUE)
{
	$search = array(
		'/[^\w\-\. ]+/u',			// Remove non safe characters
		'/\s\s+/',					// Remove extra whitespace
		'/\.\.+/', '/--+/', '/__+/'	// Remove duplicate symbols
	);

	$string = preg_replace($search, array(' ', ' ', '.', '-', '_'), $string);

	if( ! $spaces)
	{
		$string = preg_replace('/--+/', '-', str_replace(' ', '-', $string));
	}

	return trim($string, '-._ ');
}


/**
 * Create a SEO friendly URL string from a valid UTF-8 string.
 *
 * @param string $string to filter
 * @return string
 */
function sanitize_url($string)
{
	return urlencode(mb_strtolower(sanitize($string, FALSE)));
}


/**
 * Filter a valid UTF-8 string to be file name safe.
 *
 * @param string $string to filter
 * @return string
 */
function sanitize_filename($string)
{
	return sanitize($string, FALSE);
}


/**
 * Return a SQLite/MySQL/PostgreSQL datetime string
 *
 * @param int $timestamp
 */
function sql_date($timestamp = NULL)
{
	return date('Y-m-d H:i:s', $timestamp ?: time());
}


/**
 * Make a request to the given URL using cURL.
 *
 * @param string $url to request
 * @param array $options for cURL object
 * @return object
 */
function curl_request($url, array $options = NULL)
{
	$ch = curl_init($url);

	$defaults = array(
		CURLOPT_HEADER => 0,
		CURLOPT_RETURNTRANSFER => 1,
		CURLOPT_TIMEOUT => 5,
	);

	// Connection options override defaults if given
	curl_setopt_array($ch, (array) $options + $defaults);

	// Create a response object
	$object = new stdClass;

	// Get additional request info
	$object->response = curl_exec($ch);
	$object->error_code = curl_errno($ch);
	$object->error = curl_error($ch);
	$object->info = curl_getinfo($ch);

	curl_close($ch);

	return $object;
}


/**
 * Create a RecursiveDirectoryIterator object
 *
 * @param string $dir the directory to load
 * @param boolean $recursive to include subfolders
 * @return object
 */
function directory($dir, $recursive = TRUE)
{
	$i = new \RecursiveDirectoryIterator($dir);

	if( ! $recursive) return $i;

	return new \RecursiveIteratorIterator($i, \RecursiveIteratorIterator::SELF_FIRST);
}


/**
 * Make sure that a directory exists and is writable by the current PHP process.
 *
 * @param string $dir the directory to load
 * @param string $chmod value as octal
 * @return boolean
 */
function directory_is_writable($dir, $chmod = 0755)
{
	// If it doesn't exist, and can't be made
	if(! is_dir($dir) AND ! mkdir($dir, $chmod, TRUE)) return FALSE;

	// If it isn't writable, and can't be made writable
	if(! is_writable($dir) AND !chmod($dir, $chmod)) return FALSE;

	return TRUE;
}


/**
 * Convert any given variable into a SimpleXML object
 *
 * @param mixed $object variable object to convert
 * @param string $root root element name
 * @param object $xml xml object
 * @param string $unknown element name for numeric keys
 * @param string $doctype XML doctype
 */
function to_xml($object, $root = 'data', $xml = NULL, $unknown = 'element', $doctype = "<?xml version = '1.0' encoding = 'utf-8'?>")
{
	if(is_null($xml))
	{
		$xml = simplexml_load_string("$doctype<$root/>");
	}

	foreach((array) $object as $k => $v)
	{
		if(is_int($k))
		{
			$k = $unknown;
		}

		if(is_scalar($v))
		{
			$xml->addChild($k, h($v));
		}
		else
		{
			$v = (array) $v;
			$node = array_diff_key($v, array_keys(array_keys($v))) ? $xml->addChild($k) : $xml;
			self::from($v, $k, $node);
		}
	}

	return $xml;
}


/**
 * Return an IntlDateFormatter object using the current system locale
 *
 * @param string $locale string
 * @param integer $datetype IntlDateFormatter constant
 * @param integer $timetype IntlDateFormatter constant
 * @param string $timezone Time zone ID, default is system default
 * @return IntlDateFormatter
 */
function __date($locale = NULL, $datetype = IntlDateFormatter::MEDIUM, $timetype = IntlDateFormatter::SHORT, $timezone = NULL)
{
	return new IntlDateFormatter($locale ?: setlocale(LC_ALL, 0), $datetype, $timetype, $timezone);
}


/**
 * Format the given string using the current system locale
 * Basically, it's sprintf on i18n steroids.
 *
 * @param string $string to parse
 * @param array $params to insert
 * @return string
 */
function __($string, array $params = NULL)
{
	return msgfmt_format_message(setlocale(LC_ALL, 0), $string, $params);
}


/**
 * Color output text for the CLI
 *
 * @param string $text to color
 * @param string $color of text
 * @param string $background color
 */
function colorize($text, $color, $bold = FALSE)
{
	// Standard CLI colors
	$colors = array_flip(array(30 => 'gray', 'red', 'green', 'yellow', 'blue', 'purple', 'cyan', 'white', 'black'));

	// Escape string with color information
	return"\033[" . ($bold ? '1' : '0') . ';' . $colors[$color] . "m$text\033[0m";
}

// End
