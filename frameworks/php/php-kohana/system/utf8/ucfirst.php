<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * UTF8::ucfirst
 *
 * @package    Kohana
 * @author     Kohana Team
 * @copyright  (c) 2007-2012 Kohana Team
 * @copyright  (c) 2005 Harry Fuecks
 * @license    http://www.gnu.org/licenses/old-licenses/lgpl-2.1.txt
 */
function _ucfirst($str)
{
	if (UTF8::is_ascii($str))
		return ucfirst($str);

	preg_match('/^(.?)(.*)$/us', $str, $matches);
	return UTF8::strtoupper($matches[1]).$matches[2];
}