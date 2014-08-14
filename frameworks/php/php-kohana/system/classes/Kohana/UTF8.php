<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * A port of [phputf8](http://phputf8.sourceforge.net/) to a unified set
 * of files. Provides multi-byte aware replacement string functions.
 *
 * For UTF-8 support to work correctly, the following requirements must be met:
 *
 * - PCRE needs to be compiled with UTF-8 support (--enable-utf8)
 * - Support for [Unicode properties](http://php.net/manual/reference.pcre.pattern.modifiers.php)
 *   is highly recommended (--enable-unicode-properties)
 * - UTF-8 conversion will be much more reliable if the
 *   [iconv extension](http://php.net/iconv) is loaded
 * - The [mbstring extension](http://php.net/mbstring) is highly recommended,
 *   but must not be overloading string functions
 *
 * [!!] This file is licensed differently from the rest of Kohana. As a port of
 * [phputf8](http://phputf8.sourceforge.net/), this file is released under the LGPL.
 *
 * @package    Kohana
 * @category   Base
 * @author     Kohana Team
 * @copyright  (c) 2007-2012 Kohana Team
 * @copyright  (c) 2005 Harry Fuecks
 * @license    http://www.gnu.org/licenses/old-licenses/lgpl-2.1.txt
 */
class Kohana_UTF8 {

	/**
	 * @var  boolean  Does the server support UTF-8 natively?
	 */
	public static $server_utf8 = NULL;

	/**
	 * @var  array  List of called methods that have had their required file included.
	 */
	public static $called = array();

	/**
	 * Recursively cleans arrays, objects, and strings. Removes ASCII control
	 * codes and converts to the requested charset while silently discarding
	 * incompatible characters.
	 *
	 *     UTF8::clean($_GET); // Clean GET data
	 *
	 * [!!] This method requires [Iconv](http://php.net/iconv)
	 *
	 * @param   mixed   $var        variable to clean
	 * @param   string  $charset    character set, defaults to Kohana::$charset
	 * @return  mixed
	 * @uses    UTF8::strip_ascii_ctrl
	 * @uses    UTF8::is_ascii
	 */
	public static function clean($var, $charset = NULL)
	{
		if ( ! $charset)
		{
			// Use the application character set
			$charset = Kohana::$charset;
		}

		if (is_array($var) OR is_object($var))
		{
			foreach ($var as $key => $val)
			{
				// Recursion!
				$var[self::clean($key)] = self::clean($val);
			}
		}
		elseif (is_string($var) AND $var !== '')
		{
			// Remove control characters
			$var = self::strip_ascii_ctrl($var);

			if ( ! self::is_ascii($var))
			{
				// Disable notices
				$error_reporting = error_reporting(~E_NOTICE);

				// iconv is expensive, so it is only used when needed
				$var = iconv($charset, $charset.'//IGNORE', $var);

				// Turn notices back on
				error_reporting($error_reporting);
			}
		}

		return $var;
	}

	/**
	 * Tests whether a string contains only 7-bit ASCII bytes. This is used to
	 * determine when to use native functions or UTF-8 functions.
	 *
	 *     $ascii = UTF8::is_ascii($str);
	 *
	 * @param   mixed   $str    string or array of strings to check
	 * @return  boolean
	 */
	public static function is_ascii($str)
	{
		if (is_array($str))
		{
			$str = implode($str);
		}

		return ! preg_match('/[^\x00-\x7F]/S', $str);
	}

	/**
	 * Strips out device control codes in the ASCII range.
	 *
	 *     $str = UTF8::strip_ascii_ctrl($str);
	 *
	 * @param   string  $str    string to clean
	 * @return  string
	 */
	public static function strip_ascii_ctrl($str)
	{
		return preg_replace('/[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]+/S', '', $str);
	}

	/**
	 * Strips out all non-7bit ASCII bytes.
	 *
	 *     $str = UTF8::strip_non_ascii($str);
	 *
	 * @param   string  $str    string to clean
	 * @return  string
	 */
	public static function strip_non_ascii($str)
	{
		return preg_replace('/[^\x00-\x7F]+/S', '', $str);
	}

	/**
	 * Replaces special/accented UTF-8 characters by ASCII-7 "equivalents".
	 *
	 *     $ascii = UTF8::transliterate_to_ascii($utf8);
	 *
	 * @author  Andreas Gohr <andi@splitbrain.org>
	 * @param   string  $str    string to transliterate
	 * @param   integer $case   -1 lowercase only, +1 uppercase only, 0 both cases
	 * @return  string
	 */
	public static function transliterate_to_ascii($str, $case = 0)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _transliterate_to_ascii($str, $case);
	}

	/**
	 * Returns the length of the given string. This is a UTF8-aware version
	 * of [strlen](http://php.net/strlen).
	 *
	 *     $length = UTF8::strlen($str);
	 *
	 * @param   string  $str    string being measured for length
	 * @return  integer
	 * @uses    UTF8::$server_utf8
	 */
	public static function strlen($str)
	{
		if (UTF8::$server_utf8)
			return mb_strlen($str, Kohana::$charset);

		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _strlen($str);
	}

	/**
	 * Finds position of first occurrence of a UTF-8 string. This is a
	 * UTF8-aware version of [strpos](http://php.net/strpos).
	 *
	 *     $position = UTF8::strpos($str, $search);
	 *
	 * @author  Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $str    haystack
	 * @param   string  $search needle
	 * @param   integer $offset offset from which character in haystack to start searching
	 * @return  integer position of needle
	 * @return  boolean FALSE if the needle is not found
	 * @uses    UTF8::$server_utf8
	 */
	public static function strpos($str, $search, $offset = 0)
	{
		if (UTF8::$server_utf8)
			return mb_strpos($str, $search, $offset, Kohana::$charset);

		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _strpos($str, $search, $offset);
	}

	/**
	 * Finds position of last occurrence of a char in a UTF-8 string. This is
	 * a UTF8-aware version of [strrpos](http://php.net/strrpos).
	 *
	 *     $position = UTF8::strrpos($str, $search);
	 *
	 * @author  Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $str    haystack
	 * @param   string  $search needle
	 * @param   integer $offset offset from which character in haystack to start searching
	 * @return  integer position of needle
	 * @return  boolean FALSE if the needle is not found
	 * @uses    UTF8::$server_utf8
	 */
	public static function strrpos($str, $search, $offset = 0)
	{
		if (UTF8::$server_utf8)
			return mb_strrpos($str, $search, $offset, Kohana::$charset);

		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _strrpos($str, $search, $offset);
	}

	/**
	 * Returns part of a UTF-8 string. This is a UTF8-aware version
	 * of [substr](http://php.net/substr).
	 *
	 *     $sub = UTF8::substr($str, $offset);
	 *
	 * @author  Chris Smith <chris@jalakai.co.uk>
	 * @param   string  $str    input string
	 * @param   integer $offset offset
	 * @param   integer $length length limit
	 * @return  string
	 * @uses    UTF8::$server_utf8
	 * @uses    Kohana::$charset
	 */
	public static function substr($str, $offset, $length = NULL)
	{
		if (UTF8::$server_utf8)
			return ($length === NULL)
				? mb_substr($str, $offset, mb_strlen($str), Kohana::$charset)
				: mb_substr($str, $offset, $length, Kohana::$charset);

		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _substr($str, $offset, $length);
	}

	/**
	 * Replaces text within a portion of a UTF-8 string. This is a UTF8-aware
	 * version of [substr_replace](http://php.net/substr_replace).
	 *
	 *     $str = UTF8::substr_replace($str, $replacement, $offset);
	 *
	 * @author  Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $str            input string
	 * @param   string  $replacement    replacement string
	 * @param   integer $offset         offset
	 * @return  string
	 */
	public static function substr_replace($str, $replacement, $offset, $length = NULL)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _substr_replace($str, $replacement, $offset, $length);
	}

	/**
	 * Makes a UTF-8 string lowercase. This is a UTF8-aware version
	 * of [strtolower](http://php.net/strtolower).
	 *
	 *     $str = UTF8::strtolower($str);
	 *
	 * @author  Andreas Gohr <andi@splitbrain.org>
	 * @param   string  $str    mixed case string
	 * @return  string
	 * @uses    UTF8::$server_utf8
	 */
	public static function strtolower($str)
	{
		if (UTF8::$server_utf8)
			return mb_strtolower($str, Kohana::$charset);

		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _strtolower($str);
	}

	/**
	 * Makes a UTF-8 string uppercase. This is a UTF8-aware version
	 * of [strtoupper](http://php.net/strtoupper).
	 *
	 * @author  Andreas Gohr <andi@splitbrain.org>
	 * @param   string  $str    mixed case string
	 * @return  string
	 * @uses    UTF8::$server_utf8
	 * @uses    Kohana::$charset
	 */
	public static function strtoupper($str)
	{
		if (UTF8::$server_utf8)
			return mb_strtoupper($str, Kohana::$charset);

		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _strtoupper($str);
	}

	/**
	 * Makes a UTF-8 string's first character uppercase. This is a UTF8-aware
	 * version of [ucfirst](http://php.net/ucfirst).
	 *
	 *     $str = UTF8::ucfirst($str);
	 *
	 * @author  Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $str    mixed case string
	 * @return  string
	 */
	public static function ucfirst($str)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _ucfirst($str);
	}

	/**
	 * Makes the first character of every word in a UTF-8 string uppercase.
	 * This is a UTF8-aware version of [ucwords](http://php.net/ucwords).
	 *
	 *     $str = UTF8::ucwords($str);
	 *
	 * @author  Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $str    mixed case string
	 * @return  string
	 * @uses    UTF8::$server_utf8
	 */
	public static function ucwords($str)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _ucwords($str);
	}

	/**
	 * Case-insensitive UTF-8 string comparison. This is a UTF8-aware version
	 * of [strcasecmp](http://php.net/strcasecmp).
	 *
	 *     $compare = UTF8::strcasecmp($str1, $str2);
	 *
	 * @author  Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $str1   string to compare
	 * @param   string  $str2   string to compare
	 * @return  integer less than 0 if str1 is less than str2
	 * @return  integer greater than 0 if str1 is greater than str2
	 * @return  integer 0 if they are equal
	 */
	public static function strcasecmp($str1, $str2)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _strcasecmp($str1, $str2);
	}

	/**
	 * Returns a string or an array with all occurrences of search in subject
	 * (ignoring case) and replaced with the given replace value. This is a
	 * UTF8-aware version of [str_ireplace](http://php.net/str_ireplace).
	 *
	 * [!!] This function is very slow compared to the native version. Avoid
	 * using it when possible.
	 *
	 * @author  Harry Fuecks <hfuecks@gmail.com
	 * @param   string|array    $search     text to replace
	 * @param   string|array    $replace    replacement text
	 * @param   string|array    $str        subject text
	 * @param   integer         $count      number of matched and replaced needles will be returned via this parameter which is passed by reference
	 * @return  string  if the input was a string
	 * @return  array   if the input was an array
	 */
	public static function str_ireplace($search, $replace, $str, & $count = NULL)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _str_ireplace($search, $replace, $str, $count);
	}

	/**
	 * Case-insenstive UTF-8 version of strstr. Returns all of input string
	 * from the first occurrence of needle to the end. This is a UTF8-aware
	 * version of [stristr](http://php.net/stristr).
	 *
	 *     $found = UTF8::stristr($str, $search);
	 *
	 * @author Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $str    input string
	 * @param   string  $search needle
	 * @return  string  matched substring if found
	 * @return  FALSE   if the substring was not found
	 */
	public static function stristr($str, $search)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _stristr($str, $search);
	}

	/**
	 * Finds the length of the initial segment matching mask. This is a
	 * UTF8-aware version of [strspn](http://php.net/strspn).
	 *
	 *     $found = UTF8::strspn($str, $mask);
	 *
	 * @author Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $str    input string
	 * @param   string  $mask   mask for search
	 * @param   integer $offset start position of the string to examine
	 * @param   integer $length length of the string to examine
	 * @return  integer length of the initial segment that contains characters in the mask
	 */
	public static function strspn($str, $mask, $offset = NULL, $length = NULL)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _strspn($str, $mask, $offset, $length);
	}

	/**
	 * Finds the length of the initial segment not matching mask. This is a
	 * UTF8-aware version of [strcspn](http://php.net/strcspn).
	 *
	 *     $found = UTF8::strcspn($str, $mask);
	 *
	 * @author  Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $str    input string
	 * @param   string  $mask   mask for search
	 * @param   integer $offset start position of the string to examine
	 * @param   integer $length length of the string to examine
	 * @return  integer length of the initial segment that contains characters not in the mask
	 */
	public static function strcspn($str, $mask, $offset = NULL, $length = NULL)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _strcspn($str, $mask, $offset, $length);
	}

	/**
	 * Pads a UTF-8 string to a certain length with another string. This is a
	 * UTF8-aware version of [str_pad](http://php.net/str_pad).
	 *
	 *     $str = UTF8::str_pad($str, $length);
	 *
	 * @author  Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $str                input string
	 * @param   integer $final_str_length   desired string length after padding
	 * @param   string  $pad_str            string to use as padding
	 * @param   string  $pad_type           padding type: STR_PAD_RIGHT, STR_PAD_LEFT, or STR_PAD_BOTH
	 * @return  string
	 */
	public static function str_pad($str, $final_str_length, $pad_str = ' ', $pad_type = STR_PAD_RIGHT)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _str_pad($str, $final_str_length, $pad_str, $pad_type);
	}

	/**
	 * Converts a UTF-8 string to an array. This is a UTF8-aware version of
	 * [str_split](http://php.net/str_split).
	 *
	 *     $array = UTF8::str_split($str);
	 *
	 * @author  Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $str            input string
	 * @param   integer $split_length   maximum length of each chunk
	 * @return  array
	 */
	public static function str_split($str, $split_length = 1)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _str_split($str, $split_length);
	}

	/**
	 * Reverses a UTF-8 string. This is a UTF8-aware version of [strrev](http://php.net/strrev).
	 *
	 *     $str = UTF8::strrev($str);
	 *
	 * @author  Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $str    string to be reversed
	 * @return  string
	 */
	public static function strrev($str)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _strrev($str);
	}

	/**
	 * Strips whitespace (or other UTF-8 characters) from the beginning and
	 * end of a string. This is a UTF8-aware version of [trim](http://php.net/trim).
	 *
	 *     $str = UTF8::trim($str);
	 *
	 * @author  Andreas Gohr <andi@splitbrain.org>
	 * @param   string  $str        input string
	 * @param   string  $charlist   string of characters to remove
	 * @return  string
	 */
	public static function trim($str, $charlist = NULL)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _trim($str, $charlist);
	}

	/**
	 * Strips whitespace (or other UTF-8 characters) from the beginning of
	 * a string. This is a UTF8-aware version of [ltrim](http://php.net/ltrim).
	 *
	 *     $str = UTF8::ltrim($str);
	 *
	 * @author  Andreas Gohr <andi@splitbrain.org>
	 * @param   string  $str        input string
	 * @param   string  $charlist   string of characters to remove
	 * @return  string
	 */
	public static function ltrim($str, $charlist = NULL)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _ltrim($str, $charlist);
	}

	/**
	 * Strips whitespace (or other UTF-8 characters) from the end of a string.
	 * This is a UTF8-aware version of [rtrim](http://php.net/rtrim).
	 *
	 *     $str = UTF8::rtrim($str);
	 *
	 * @author  Andreas Gohr <andi@splitbrain.org>
	 * @param   string  $str        input string
	 * @param   string  $charlist   string of characters to remove
	 * @return  string
	 */
	public static function rtrim($str, $charlist = NULL)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _rtrim($str, $charlist);
	}

	/**
	 * Returns the unicode ordinal for a character. This is a UTF8-aware
	 * version of [ord](http://php.net/ord).
	 *
	 *     $digit = UTF8::ord($character);
	 *
	 * @author  Harry Fuecks <hfuecks@gmail.com>
	 * @param   string  $chr    UTF-8 encoded character
	 * @return  integer
	 */
	public static function ord($chr)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _ord($chr);
	}

	/**
	 * Takes an UTF-8 string and returns an array of ints representing the Unicode characters.
	 * Astral planes are supported i.e. the ints in the output can be > 0xFFFF.
	 * Occurrences of the BOM are ignored. Surrogates are not allowed.
	 *
	 *     $array = UTF8::to_unicode($str);
	 *
	 * The Original Code is Mozilla Communicator client code.
	 * The Initial Developer of the Original Code is Netscape Communications Corporation.
	 * Portions created by the Initial Developer are Copyright (C) 1998 the Initial Developer.
	 * Ported to PHP by Henri Sivonen <hsivonen@iki.fi>, see <http://hsivonen.iki.fi/php-utf8/>
	 * Slight modifications to fit with phputf8 library by Harry Fuecks <hfuecks@gmail.com>
	 *
	 * @param   string  $str    UTF-8 encoded string
	 * @return  array   unicode code points
	 * @return  FALSE   if the string is invalid
	 */
	public static function to_unicode($str)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _to_unicode($str);
	}

	/**
	 * Takes an array of ints representing the Unicode characters and returns a UTF-8 string.
	 * Astral planes are supported i.e. the ints in the input can be > 0xFFFF.
	 * Occurrances of the BOM are ignored. Surrogates are not allowed.
	 *
	 *     $str = UTF8::to_unicode($array);
	 *
	 * The Original Code is Mozilla Communicator client code.
	 * The Initial Developer of the Original Code is Netscape Communications Corporation.
	 * Portions created by the Initial Developer are Copyright (C) 1998 the Initial Developer.
	 * Ported to PHP by Henri Sivonen <hsivonen@iki.fi>, see http://hsivonen.iki.fi/php-utf8/
	 * Slight modifications to fit with phputf8 library by Harry Fuecks <hfuecks@gmail.com>.
	 *
	 * @param   array   $str    unicode code points representing a string
	 * @return  string  utf8 string of characters
	 * @return  boolean FALSE if a code point cannot be found
	 */
	public static function from_unicode($arr)
	{
		if ( ! isset(self::$called[__FUNCTION__]))
		{
			require Kohana::find_file('utf8', __FUNCTION__);

			// Function has been called
			self::$called[__FUNCTION__] = TRUE;
		}

		return _from_unicode($arr);
	}

} // End UTF8

if (Kohana_UTF8::$server_utf8 === NULL)
{
	// Determine if this server supports UTF-8 natively
	Kohana_UTF8::$server_utf8 = extension_loaded('mbstring');
}
