<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * UTF8::ucwords
 *
 * @package    Kohana
 * @author     Kohana Team
 * @copyright  (c) 2007-2012 Kohana Team
 * @copyright  (c) 2005 Harry Fuecks
 * @license    http://www.gnu.org/licenses/old-licenses/lgpl-2.1.txt
 */
function _ucwords($str)
{
	if (UTF8::is_ascii($str))
		return ucwords($str);

	// [\x0c\x09\x0b\x0a\x0d\x20] matches form feeds, horizontal tabs, vertical tabs, linefeeds and carriage returns.
	// This corresponds to the definition of a 'word' defined at http://php.net/ucwords
	return preg_replace(
		'/(?<=^|[\x0c\x09\x0b\x0a\x0d\x20])[^\x0c\x09\x0b\x0a\x0d\x20]/ue',
		'UTF8::strtoupper(\'$0\')',
		$str
	);
}