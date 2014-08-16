<?php
/** @package    verysimple::Util */

/**
 * Formatter for formatting Exceptions and stack traces
 *
 * @package    verysimple::String
 * @author Jason Hinkle
 * @copyright  1997-2008 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
class ExceptionFormatter
{
	
	/**
	 * This is a utility function for tracing errors.  It will return a string that
	 * displys the current execution stack
	 * 
	 * @param string $msg a debugging message to include
	 * @param int $depth how far to go back in the stack (default = unlimited)
	 * @param string $join the delimiter between lines
	 * @param bool $show_lines true to include line numbers
	 */
	static function GetTraceAsString($msg = "DEBUG", $depth = 0, $join = " :: ", $show_lines = true)
	{
		$error = new Exception($msg);
		return self::FormatTrace($error->getTrace(), $depth, $join, $show_lines);
	}
	
	/**
	 * Formats the debug_backtrace array into a printable string.  
	 * You can create a debug traceback using $exception->getTrace() 
	 * or using the php debug_backtrace() function
	 *
	 * @access public
	 * @param array debug_backtrace.  For example: debug_backtrace() -or- $exception->getTrace()
	 * @param int $depth how far to go back in the stack (default = unlimited)
	 * @param string $join the delimiter between lines
	 * @param bool $show_lines true to include line numbers
	 */
	static function FormatTrace($tb, $depth = 0, $join = " :: ", $show_lines = true)
	{
		$msg = "";
		$delim = "";
		
		$calling_function = "";
		$calling_line = "[?]";
		$levels = count($tb);
		
		if ($depth == 0) $depth = $levels;
		
		for ($x = $levels; $x > 0; $x--)
		{
			$stack = $tb[$x-1];
			$s_file = isset($stack['file']) ? basename($stack['file']) : "[?]";
			$s_line = isset($stack['line']) ? $stack['line'] : "[?]";
			$s_function = isset($stack['function']) ? $stack['function'] : "";
			$s_class = isset($stack['class']) ? $stack['class'] : "";
			$s_type = isset($stack['type']) ? $stack['type'] : "";
			
			if ($depth >= $x)
			{ 
				$msg .= $delim . "$calling_function" . ($show_lines ? " ($s_file Line $s_line)" : "");
				$delim = $join;
			}

			$calling_function = $s_class . $s_type . $s_function;

		}
		
		return $msg;
		
	}
	
}

?>