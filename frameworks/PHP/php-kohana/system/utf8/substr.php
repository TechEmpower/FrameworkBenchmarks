<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * UTF8::substr
 *
 * @package    Kohana
 * @author     Kohana Team
 * @copyright  (c) 2007-2012 Kohana Team
 * @copyright  (c) 2005 Harry Fuecks
 * @license    http://www.gnu.org/licenses/old-licenses/lgpl-2.1.txt
 */
function _substr($str, $offset, $length = NULL)
{
	if (UTF8::is_ascii($str))
		return ($length === NULL) ? substr($str, $offset) : substr($str, $offset, $length);

	// Normalize params
	$str    = (string) $str;
	$strlen = UTF8::strlen($str);
	$offset = (int) ($offset < 0) ? max(0, $strlen + $offset) : $offset; // Normalize to positive offset
	$length = ($length === NULL) ? NULL : (int) $length;

	// Impossible
	if ($length === 0 OR $offset >= $strlen OR ($length < 0 AND $length <= $offset - $strlen))
		return '';

	// Whole string
	if ($offset == 0 AND ($length === NULL OR $length >= $strlen))
		return $str;

	// Build regex
	$regex = '^';

	// Create an offset expression
	if ($offset > 0)
	{
		// PCRE repeating quantifiers must be less than 65536, so repeat when necessary
		$x = (int) ($offset / 65535);
		$y = (int) ($offset % 65535);
		$regex .= ($x == 0) ? '' : ('(?:.{65535}){'.$x.'}');
		$regex .= ($y == 0) ? '' : ('.{'.$y.'}');
	}

	// Create a length expression
	if ($length === NULL)
	{
		$regex .= '(.*)'; // No length set, grab it all
	}
	// Find length from the left (positive length)
	elseif ($length > 0)
	{
		// Reduce length so that it can't go beyond the end of the string
		$length = min($strlen - $offset, $length);

		$x = (int) ($length / 65535);
		$y = (int) ($length % 65535);
		$regex .= '(';
		$regex .= ($x == 0) ? '' : ('(?:.{65535}){'.$x.'}');
		$regex .= '.{'.$y.'})';
	}
	// Find length from the right (negative length)
	else
	{
		$x = (int) (-$length / 65535);
		$y = (int) (-$length % 65535);
		$regex .= '(.*)';
		$regex .= ($x == 0) ? '' : ('(?:.{65535}){'.$x.'}');
		$regex .= '.{'.$y.'}';
	}

	preg_match('/'.$regex.'/us', $str, $matches);
	return $matches[1];
}