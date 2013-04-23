<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * UTF8::from_unicode
 *
 * @package    Kohana
 * @author     Kohana Team
 * @copyright  (c) 2007-2012 Kohana Team
 * @copyright  (c) 2005 Harry Fuecks
 * @license    http://www.gnu.org/licenses/old-licenses/lgpl-2.1.txt
 */
function _from_unicode($arr)
{
	ob_start();

	$keys = array_keys($arr);

	foreach ($keys as $k)
	{
		// ASCII range (including control chars)
		if (($arr[$k] >= 0) AND ($arr[$k] <= 0x007f))
		{
			echo chr($arr[$k]);
		}
		// 2 byte sequence
		elseif ($arr[$k] <= 0x07ff)
		{
			echo chr(0xc0 | ($arr[$k] >> 6));
			echo chr(0x80 | ($arr[$k] & 0x003f));
		}
		// Byte order mark (skip)
		elseif ($arr[$k] == 0xFEFF)
		{
			// nop -- zap the BOM
		}
		// Test for illegal surrogates
		elseif ($arr[$k] >= 0xD800 AND $arr[$k] <= 0xDFFF)
		{
			// Found a surrogate
			throw new UTF8_Exception("UTF8::from_unicode: Illegal surrogate at index: ':index', value: ':value'", array(
				':index' => $k,
				':value' => $arr[$k],
			));
		}
		// 3 byte sequence
		elseif ($arr[$k] <= 0xffff)
		{
			echo chr(0xe0 | ($arr[$k] >> 12));
			echo chr(0x80 | (($arr[$k] >> 6) & 0x003f));
			echo chr(0x80 | ($arr[$k] & 0x003f));
		}
		// 4 byte sequence
		elseif ($arr[$k] <= 0x10ffff)
		{
			echo chr(0xf0 | ($arr[$k] >> 18));
			echo chr(0x80 | (($arr[$k] >> 12) & 0x3f));
			echo chr(0x80 | (($arr[$k] >> 6) & 0x3f));
			echo chr(0x80 | ($arr[$k] & 0x3f));
		}
		// Out of range
		else
		{
			throw new UTF8_Exception("UTF8::from_unicode: Codepoint out of Unicode range at index: ':index', value: ':value'", array(
				':index' => $k,
				':value' => $arr[$k],
			));
		}
	}

	$result = ob_get_contents();
	ob_end_clean();
	return $result;
}
