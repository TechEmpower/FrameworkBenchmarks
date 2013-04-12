<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * UTF8::strcspn
 *
 * @package    Kohana
 * @author     Kohana Team
 * @copyright  (c) 2007-2012 Kohana Team
 * @copyright  (c) 2005 Harry Fuecks
 * @license    http://www.gnu.org/licenses/old-licenses/lgpl-2.1.txt
 */
function _strcspn($str, $mask, $offset = NULL, $length = NULL)
{
	if ($str == '' OR $mask == '')
		return 0;

	if (UTF8::is_ascii($str) AND UTF8::is_ascii($mask))
		return ($offset === NULL) ? strcspn($str, $mask) : (($length === NULL) ? strcspn($str, $mask, $offset) : strcspn($str, $mask, $offset, $length));

	if ($offset !== NULL OR $length !== NULL)
	{
		$str = UTF8::substr($str, $offset, $length);
	}

	// Escape these characters:  - [ ] . : \ ^ /
	// The . and : are escaped to prevent possible warnings about POSIX regex elements
	$mask = preg_replace('#[-[\].:\\\\^/]#', '\\\\$0', $mask);
	preg_match('/^[^'.$mask.']+/u', $str, $matches);

	return isset($matches[0]) ? UTF8::strlen($matches[0]) : 0;
}