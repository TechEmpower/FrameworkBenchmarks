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
 * Numeric helper class. Provides additional formatting methods for working with
 * numeric values.
 *
 * Credit is left where credit is due.
 *
 * Techniques and inspiration were taken from all over, including:
 *	Kohana Framework: kohanaframework.org
 *	CakePHP: cakephp.org
 *
 * @package		Fuel
 * @category	Core
 * @author      Chase "Syntaqx" Hutchins
 */
class Num
{

	/**
	 * Cached byte units
	 *
	 * @var   array
	 */
	protected static $byte_units;

	/**
	 * Cached configuration values
	 *
	 * @var   array
	 */
	protected static $config;

	/**
	 * Class initialization callback
	 *
	 * @return   void
	 */
	public static function _init()
	{
		\Lang::load('byte_units', true);

		static::$config     = \Config::load('num', true);
		static::$byte_units = \Lang::get('byte_units');
	}

	/**
	 * Converts a file size number to a byte value. File sizes are defined in
	 * the format: SB, where S is the size (1, 8.5, 300, etc.) and B is the
	 * byte unit (K, MiB, GB, etc.). All valid byte units are defined in
	 * static::$byte_units
	 *
	 * Usage:
	 * <code>
	 * echo Num::bytes('200K');  // 204800
	 * echo static::bytes('5MiB');  // 5242880
	 * echo static::bytes('1000');  // 1000
	 * echo static::bytes('2.5GB'); // 2684354560
	 * </code>
	 *
	 * @author     Kohana Team
	 * @copyright  (c) 2009-2011 Kohana Team
	 * @license    http://kohanaframework.org/license
	 * @param      string   file size in SB format
	 * @return     float
	 */
	public static function bytes($size = 0)
	{
		// Prepare the size
		$size = trim((string) $size);

		// Construct an OR list of byte units for the regex
		$accepted = implode('|', array_keys(static::$byte_units));

		// Construct the regex pattern for verifying the size format
		$pattern = '/^([0-9]+(?:\.[0-9]+)?)('.$accepted.')?$/Di';

		// Verify the size format and store the matching parts
		if (!preg_match($pattern, $size, $matches))
		{
			throw new \Exception('The byte unit size, "'.$size.'", is improperly formatted.');
		}

		// Find the float value of the size
		$size = (float) $matches[1];

		// Find the actual unit, assume B if no unit specified
		$unit = \Arr::get($matches, 2, 'B');

		// Convert the size into bytes
		$bytes = $size * pow(2, static::$byte_units[$unit]);

		return $bytes;
	}

	/**
	 * Converts a number of bytes to a human readable number by taking the
	 * number of that unit that the bytes will go into it. Supports TB value.
	 *
	 * Note: Integers in PHP are limited to 32 bits, unless they are on 64 bit
	 * architectures, then they have 64 bit size. If you need to place the
	 * larger size then what the PHP integer type will hold, then use a string.
	 * It will be converted to a double, which should always have 64 bit length.
	 *
	 * @param   integer
	 * @param   integer
	 * @return  boolean|string
	 */
	public static function format_bytes($bytes = 0, $decimals = 0)
	{
		$quant = array(
			'TB' => 1099511627776,  // pow( 1024, 4)
			'GB' => 1073741824,     // pow( 1024, 3)
			'MB' => 1048576,        // pow( 1024, 2)
			'kB' => 1024,           // pow( 1024, 1)
			'B ' => 1,              // pow( 1024, 0)
		);

		foreach ($quant as $unit => $mag )
		{
			if (doubleval($bytes) >= $mag)
			{
				return sprintf('%01.'.$decimals.'f', ($bytes / $mag)).' '.$unit;
			}
		}

		return false;
	}

	/**
	 * Converts a number into a more readable human-type number.
	 *
	 * Usage:
	 * <code>
	 * echo Num::quantity(7000); // 7K
	 * echo Num::quantity(7500); // 8K
	 * echo Num::quantity(7500, 1); // 7.5K
	 * </code>
	 *
	 * @param   integer
	 * @param   integer
	 * @return  string
	 */
	public static function quantity($num, $decimals = 0)
	{
		if ($num >= 1000 && $num < 1000000)
		{
			return sprintf('%01.'.$decimals.'f', (sprintf('%01.0f', $num) / 1000)).'K';
		}
		elseif ($num >= 1000000 && $num < 1000000000)
		{
			return sprintf('%01.'.$decimals.'f', (sprintf('%01.0f', $num) / 1000000)).'M';
		}
		elseif ($num >= 1000000000)
		{
			return sprintf('%01.'.$decimals.'f', (sprintf('%01.0f', $num) / 1000000000)).'B';
		}

		return $num;
	}

	/**
	 * Formats a number by injecting non-numeric characters in a specified
	 * format into the string in the positions they appear in the format.
	 *
	 * Usage:
	 * <code>
	 * echo Num::format('1234567890', '(000) 000-0000'); // (123) 456-7890
	 * echo Num::format('1234567890', '000.000.0000'); // 123.456.7890
	 * </code>
	 *
	 * @link    http://snippets.symfony-project.org/snippet/157
	 * @param   string     the string to format
	 * @param   string     the format to apply
	 * @return  string
	 */
	public static function format($string = '', $format = '')
	{
		if(empty($format) or empty($string))
		{
			return $string;
		}

		$result = '';
		$fpos = 0;
		$spos = 0;

		while ((strlen($format) - 1) >= $fpos)
		{
			if (ctype_alnum(substr($format, $fpos, 1)))
			{
				$result .= substr($string, $spos, 1);
				$spos++;
			}
			else
			{
				$result .= substr($format, $fpos, 1);
			}

			$fpos++;
		}

		return $result;
	}

	/**
	 * Transforms a number by masking characters in a specified mask format, and
	 * ignoring characters that should be injected into the string without
	 * matching a character from the original string (defaults to space).
	 *
	 * Usage:
	 * <code>
	 * echo Num::mask_string('1234567812345678', '************0000'); ************5678
	 * echo Num::mask_string('1234567812345678', '**** **** **** 0000'); // **** **** **** 5678
	 * echo Num::mask_string('1234567812345678', '**** - **** - **** - 0000', ' -'); // **** - **** - **** - 5678
	 * </code>
	 *
	 * @link    http://snippets.symfony-project.org/snippet/157
	 * @param   string     the string to transform
	 * @param   string     the mask format
	 * @param   string     a string (defaults to a single space) containing characters to ignore in the format
	 * @return  string     the masked string
	 */
	public static function mask_string($string = '', $format = '', $ignore = ' ')
	{
		if(empty($format) or empty($string))
		{
			return $string;
		}

		$result = '';
		$fpos = 0;
		$spos = 0;

		while ((strlen($format) - 1) >= $fpos)
		{
			if (ctype_alnum(substr($format, $fpos, 1)))
			{
				$result .= substr($string, $spos, 1);
				$spos++;
			}
			else
			{
				$result .= substr($format, $fpos, 1);

				if (strpos($ignore, substr($format, $fpos, 1)) === false)
				{
					++$spos;
				}
			}

			++$fpos;
		}

		return $result;
	}

	/**
	 * Formats a phone number.
	 *
	 * @link    http://snippets.symfony-project.org/snippet/157
	 * @param   string the unformatted phone number to format
	 * @param   string the format to use, defaults to '(000) 000-0000'
	 * @return  string the formatted string
	 * @see     format
	 */
	public static function format_phone($string = '', $format = null)
	{
		is_null($format) and $format = static::$config['formatting']['phone'];
		return static::format($string, $format);
	}

	/**
	 * Formats a variable length phone number, using a standard format.
	 *
	 * Usage:
	 * <code>
	 * echo Num::smart_format_phone('1234567'); // 123-4567
	 * echo Num::smart_format_phone('1234567890'); // (123) 456-7890
	 * echo Num::smart_format_phone('91234567890'); // 9 (123) 456-7890
	 * echo Num::smart_format_phone('123456'); // => 123456
	 * </code>
	 *
	 * @param   string     the unformatted phone number to format
	 * @see     format
	 */
	public static function smart_format_phone($string)
	{
		$formats = static::$config['formatting']['smart_phone'];

		if(is_array($formats) and isset($formats[strlen($string)]))
		{
			return static::format($string, $formats[strlen($string)]);
		}

		return $string;
	}

	/**
	 * Formats a credit card expiration string. Expects 4-digit string (MMYY).
	 *
	 * @param   string     the unformatted expiration string to format
	 * @param   string     the format to use, defaults to '00-00'
	 * @see     format
	 */
	public static function format_exp($string, $format = null)
	{
		is_null($format) and $format = static::$config['formatting']['exp'];
		return static::format($string, $format);
	}

	/**
	 * Formats (masks) a credit card.
	 *
	 * @param   string     the unformatted credit card number to format
	 * @param   string     the format to use, defaults to '**** **** **** 0000'
	 * @see     mask_string
	 */
	public static function mask_credit_card($string, $format = null)
	{
		is_null($format) and $format = static::$config['formatting']['credit_card'];
		return static::mask_string($string, $format);
	}
}

/* End of file num.php */
