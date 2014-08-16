<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * UTF8::str_ireplace
 *
 * @package    Kohana
 * @author     Kohana Team
 * @copyright  (c) 2007-2012 Kohana Team
 * @copyright  (c) 2005 Harry Fuecks
 * @license    http://www.gnu.org/licenses/old-licenses/lgpl-2.1.txt
 */
function _str_ireplace($search, $replace, $str, & $count = NULL)
{
	if (UTF8::is_ascii($search) AND UTF8::is_ascii($replace) AND UTF8::is_ascii($str))
		return str_ireplace($search, $replace, $str, $count);

	if (is_array($str))
	{
		foreach ($str as $key => $val)
		{
			$str[$key] = UTF8::str_ireplace($search, $replace, $val, $count);
		}
		return $str;
	}

	if (is_array($search))
	{
		$keys = array_keys($search);

		foreach ($keys as $k)
		{
			if (is_array($replace))
			{
				if (array_key_exists($k, $replace))
				{
					$str = UTF8::str_ireplace($search[$k], $replace[$k], $str, $count);
				}
				else
				{
					$str = UTF8::str_ireplace($search[$k], '', $str, $count);
				}
			}
			else
			{
				$str = UTF8::str_ireplace($search[$k], $replace, $str, $count);
			}
		}
		return $str;
	}

	$search = UTF8::strtolower($search);
	$str_lower = UTF8::strtolower($str);

	$total_matched_strlen = 0;
	$i = 0;

	while (preg_match('/(.*?)'.preg_quote($search, '/').'/s', $str_lower, $matches))
	{
		$matched_strlen = strlen($matches[0]);
		$str_lower = substr($str_lower, $matched_strlen);

		$offset = $total_matched_strlen + strlen($matches[1]) + ($i * (strlen($replace) - 1));
		$str = substr_replace($str, $replace, $offset, strlen($search));

		$total_matched_strlen += $matched_strlen;
		$i++;
	}

	$count += $i;
	return $str;
}
