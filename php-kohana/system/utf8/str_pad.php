<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * UTF8::str_pad
 *
 * @package    Kohana
 * @author     Kohana Team
 * @copyright  (c) 2007-2012 Kohana Team
 * @copyright  (c) 2005 Harry Fuecks
 * @license    http://www.gnu.org/licenses/old-licenses/lgpl-2.1.txt
 */
function _str_pad($str, $final_str_length, $pad_str = ' ', $pad_type = STR_PAD_RIGHT)
{
	if (UTF8::is_ascii($str) AND UTF8::is_ascii($pad_str))
		return str_pad($str, $final_str_length, $pad_str, $pad_type);

	$str_length = UTF8::strlen($str);

	if ($final_str_length <= 0 OR $final_str_length <= $str_length)
		return $str;

	$pad_str_length = UTF8::strlen($pad_str);
	$pad_length = $final_str_length - $str_length;

	if ($pad_type == STR_PAD_RIGHT)
	{
		$repeat = ceil($pad_length / $pad_str_length);
		return UTF8::substr($str.str_repeat($pad_str, $repeat), 0, $final_str_length);
	}

	if ($pad_type == STR_PAD_LEFT)
	{
		$repeat = ceil($pad_length / $pad_str_length);
		return UTF8::substr(str_repeat($pad_str, $repeat), 0, floor($pad_length)).$str;
	}

	if ($pad_type == STR_PAD_BOTH)
	{
		$pad_length /= 2;
		$pad_length_left = floor($pad_length);
		$pad_length_right = ceil($pad_length);
		$repeat_left = ceil($pad_length_left / $pad_str_length);
		$repeat_right = ceil($pad_length_right / $pad_str_length);

		$pad_left = UTF8::substr(str_repeat($pad_str, $repeat_left), 0, $pad_length_left);
		$pad_right = UTF8::substr(str_repeat($pad_str, $repeat_right), 0, $pad_length_right);
		return $pad_left.$str.$pad_right;
	}

	throw new UTF8_Exception("UTF8::str_pad: Unknown padding type (:pad_type)", array(
			':pad_type' => $pad_type,
		));
}