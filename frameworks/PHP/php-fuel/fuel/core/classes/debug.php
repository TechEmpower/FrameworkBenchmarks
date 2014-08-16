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
 * Debug class
 *
 * The Debug class is a simple utility for debugging variables, objects, arrays, etc by outputting information to the display.
 *
 * @package		Fuel
 * @category	Core
 * @link		http://docs.fuelphp.com/classes/debug.html
 */
class Debug
{

	public static $max_nesting_level = 5;

	public static $js_toggle_open = false;

	protected static $js_displayed = false;

	protected static $files = array();

	/**
	 * Quick and nice way to output a mixed variable to the browser
	 *
	 * @static
	 * @access	public
	 * @return	string
	 */
	public static function dump()
	{
		if (\Fuel::$is_cli)
		{
			// no fancy flying, jump dump 'm
			foreach (func_get_args() as $arg)
			{
				var_dump($arg);
			}
		}
		else
		{
			$backtrace = debug_backtrace();

			// If being called from within, show the file above in the backtrack
			if (strpos($backtrace[0]['file'], 'core/classes/debug.php') !== FALSE)
			{
				$callee = $backtrace[1];
				$label = \Inflector::humanize($backtrace[1]['function']);
			}
			else
			{
				$callee = $backtrace[0];
				$label = 'Debug';
			}

			$arguments = func_get_args();

			$callee['file'] = \Fuel::clean_path($callee['file']);

			if ( ! static::$js_displayed)
			{
				echo <<<JS
	<script type="text/javascript">function fuel_debug_toggle(a){if(document.getElementById){if(document.getElementById(a).style.display=="none"){document.getElementById(a).style.display="block"}else{document.getElementById(a).style.display="none"}}else{if(document.layers){if(document.id.display=="none"){document.id.display="block"}else{document.id.display="none"}}else{if(document.all.id.style.display=="none"){document.all.id.style.display="block"}else{document.all.id.style.display="none"}}}};</script>
JS;
				static::$js_displayed = true;
			}
			echo '<div class="fuelphp-dump" style="font-size: 13px;background: #EEE !important; border:1px solid #666; color: #000 !important; padding:10px;">';
			echo '<h1 style="border-bottom: 1px solid #CCC; padding: 0 0 5px 0; margin: 0 0 5px 0; font: bold 120% sans-serif;">'.$callee['file'].' @ line: '.$callee['line'].'</h1>';
			echo '<pre style="overflow:auto;font-size:100%;">';

			$count = count($arguments);
			for ($i = 1; $i <= $count; $i++)
			{
				echo '<strong>Variable #'.$i.':</strong>'.PHP_EOL;
				echo static::format('', $arguments[$i - 1]);
				echo PHP_EOL.PHP_EOL;
			}

			echo "</pre>";
			echo "</div>";
		}
	}

	/**
	 * Quick and nice way to output a mixed variable to the browser
	 *
	 * @static
	 * @access	public
	 * @return	string
	 */
	public static function inspect()
	{
		$backtrace = debug_backtrace();

		// If being called from within, show the file above in the backtrack
		if (strpos($backtrace[0]['file'], 'core/classes/debug.php') !== FALSE)
		{
			$callee = $backtrace[1];
			$label = \Inflector::humanize($backtrace[1]['function']);
		}
		else
		{
			$callee = $backtrace[0];
			$label = 'Debug';
		}

		$arguments = func_get_args();
		$total_arguments = count($arguments);

		$callee['file'] = \Fuel::clean_path($callee['file']);

		if ( ! static::$js_displayed)
		{
			echo <<<JS
<script type="text/javascript">function fuel_debug_toggle(a){if(document.getElementById){if(document.getElementById(a).style.display=="none"){document.getElementById(a).style.display="block"}else{document.getElementById(a).style.display="none"}}else{if(document.layers){if(document.id.display=="none"){document.id.display="block"}else{document.id.display="none"}}else{if(document.all.id.style.display=="none"){document.all.id.style.display="block"}else{document.all.id.style.display="none"}}}};</script>
JS;
			static::$js_displayed = true;
		}
		echo '<div class="fuelphp-inspect" style="font-size: 13px;background: #EEE !important; border:1px solid #666; color: #000 !important; padding:10px;">';
		echo '<h1 style="border-bottom: 1px solid #CCC; padding: 0 0 5px 0; margin: 0 0 5px 0; font: bold 120% sans-serif;">'.$callee['file'].' @ line: '.$callee['line'].'</h1>';
		echo '<pre style="overflow:auto;font-size:100%;">';
		$i = 0;
		foreach ($arguments as $argument)
		{
			echo '<strong>'.$label.' #'.(++$i).' of '.$total_arguments.'</strong>:<br />';
				echo static::format('...', $argument);
			echo '<br />';
		}

		echo "</pre>";
		echo "</div>";
	}

	/**
	 * Formats the given $var's output in a nice looking, Foldable interface.
	 *
	 * @param	string	$name	the name of the var
	 * @param	mixed	$var	the variable
	 * @param	int		$level	the indentation level
	 * @param	string	$indent_char	the indentation character
	 * @return	string	the formatted string.
	 */
	public static function format($name, $var, $level = 0, $indent_char = '&nbsp;&nbsp;&nbsp;&nbsp;', $scope = '')
	{
		$return = str_repeat($indent_char, $level);
		if (is_array($var))
		{
			$id = 'fuel_debug_'.mt_rand();
			$return .= "<i>{$scope}</i> <strong>{$name}</strong>";
			$return .=  " (Array, ".count($var)." element".(count($var)!=1?"s":"").")";
			if (count($var) > 0 and static::$max_nesting_level > $level)
			{
				$return .= " <a href=\"javascript:fuel_debug_toggle('$id');\" title=\"Click to ".(static::$js_toggle_open?"close":"open")."\">&crarr;</a>\n";
			}
			else
			{
				$return .= "\n";
			}

			if (static::$max_nesting_level <= $level)
			{
				$return .= str_repeat($indent_char, $level + 1)."...\n";
			}
			else
			{
				$sub_return = '';
				foreach ($var as $key => $val)
				{
					$sub_return .= static::format($key, $val, $level + 1);
				}
				if (count($var) > 0)
				{
					$return .= "<span id=\"$id\" style=\"display: ".(static::$js_toggle_open?"block":"none").";\">$sub_return</span>";
				}
				else
				{
					$return .= $sub_return;
				}
			}

		}
		elseif (is_string($var))
		{
			$return .= "<i>{$scope}</i> <strong>{$name}</strong> (String): <span style=\"color:#E00000;\">\"".htmlentities($var)."\"</span> (".strlen($var)." characters)\n";
		}
		elseif (is_float($var))
		{
			$return .= "<i>{$scope}</i> <strong>{$name}</strong> (Float): {$var}\n";
		}
		elseif (is_long($var))
		{
			$return .= "<i>{$scope}</i> <strong>{$name}</strong> (Integer): {$var}\n";
		}
		elseif (is_null($var))
		{
			$return .= "<i>{$scope}</i> <strong>{$name}</strong> : null\n";
		}
		elseif (is_bool($var))
		{
			$return .= "<i>{$scope}</i> <strong>{$name}</strong> (Boolean): ".($var ? 'true' : 'false')."\n";
		}
		elseif (is_double($var))
		{
			$return .= "<i>{$scope}</i> <strong>{$name}</strong> (Double): {$var}\n";
		}
		elseif (is_object($var))
		{
			// dirty hack to get the object id
			ob_start();
			var_dump($var);
			$contents = ob_get_contents();
			strpos($contents, 'xdebug-var-dump') !== false ? preg_match('~(.*?)\)\[<i>(\d+)(.*)~', $contents, $matches) : preg_match('~object\((.*?)#(\d+)(.*)~', $contents, $matches);
			ob_end_clean();

			$id = 'fuel_debug_'.mt_rand();
			$rvar = new \ReflectionObject($var);
			$vars = $rvar->getProperties();
			$return .= "<i>{$scope}</i> <strong>{$name}</strong> (Object #".$matches[2]."): ".get_class($var);
			if (count($vars) > 0 and static::$max_nesting_level > $level)
			{
				$return .= " <a href=\"javascript:fuel_debug_toggle('$id');\" title=\"Click to ".(static::$js_toggle_open?"close":"open")."\">&crarr;</a>\n";
			}
			$return .= "\n";

			$sub_return = '';
			foreach ($rvar->getProperties() as $prop)
			{
				$prop->isPublic() or $prop->setAccessible(true);
				if ($prop->isPrivate())
				{
					$scope = 'private';
				}
				elseif ($prop->isProtected())
				{
					$scope = 'protected';
				}
				else
				{
					$scope = 'public';
				}
				if (static::$max_nesting_level <= $level)
				{
					$sub_return .= str_repeat($indent_char, $level + 1)."...\n";
				}
				else
				{
					$sub_return .= static::format($prop->name, $prop->getValue($var), $level + 1, $indent_char, $scope);
				}
			}

			if (count($vars) > 0)
			{
				$return .= "<span id=\"$id\" style=\"display: ".(static::$js_toggle_open?"block":"none").";\">$sub_return</span>";
			}
			else
			{
				$return .= $sub_return;
			}
		}
		else
		{
			$return .= "<i>{$scope}</i> <strong>{$name}</strong>: {$var}\n";
		}
		return $return;
	}

	/**
	 * Returns the debug lines from the specified file
	 *
	 * @access	protected
	 * @param	string		the file path
	 * @param	int			the line number
	 * @param	bool		whether to use syntax highlighting or not
	 * @param	int			the amount of line padding
	 * @return	array
	 */
	public static function file_lines($filepath, $line_num, $highlight = true, $padding = 5)
	{
		// deal with eval'd code
		if (strpos($filepath, 'eval()\'d code') !== false)
		{
			return '';
		}

		// We cache the entire file to reduce disk IO for multiple errors
		if ( ! isset(static::$files[$filepath]))
		{
			static::$files[$filepath] = file($filepath, FILE_IGNORE_NEW_LINES);
			array_unshift(static::$files[$filepath], '');
		}

		$start = $line_num - $padding;
		if ($start < 0)
		{
			$start = 0;
		}

		$length = ($line_num - $start) + $padding + 1;
		if (($start + $length) > count(static::$files[$filepath]) - 1)
		{
			$length = NULL;
		}

		$debug_lines = array_slice(static::$files[$filepath], $start, $length, TRUE);

		if ($highlight)
		{
			$to_replace = array('<code>', '</code>', '<span style="color: #0000BB">&lt;?php&nbsp;', "\n");
			$replace_with = array('', '', '<span style="color: #0000BB">', '');

			foreach ($debug_lines as & $line)
			{
				$line = str_replace($to_replace, $replace_with, highlight_string('<?php ' . $line, TRUE));
			}
		}

		return $debug_lines;
	}

	public static function backtrace()
	{
		return static::dump(debug_backtrace());
	}

	/**
	* Prints a list of all currently declared classes.
	*
	* @access public
	* @static
	*/
	public static function classes()
	{
		return static::dump(get_declared_classes());
	}

	/**
	* Prints a list of all currently declared interfaces (PHP5 only).
	*
	* @access public
	* @static
	*/
	public static function interfaces()
	{
		return static::dump(get_declared_interfaces());
	}

	/**
	* Prints a list of all currently included (or required) files.
	*
	* @access public
	* @static
	*/
	public static function includes()
	{
	return static::dump(get_included_files());
	}

	/**
	 * Prints a list of all currently declared functions.
	 *
	 * @access public
	 * @static
	 */
	public static function functions()
	{
		return static::dump(get_defined_functions());
	}

	/**
	 * Prints a list of all currently declared constants.
	 *
	 * @access public
	 * @static
	 */
	public static function constants()
	{
		return static::dump(get_defined_constants());
	}

	/**
	 * Prints a list of all currently loaded PHP extensions.
	 *
	 * @access public
	 * @static
	 */
	public static function extensions()
	{
		return static::dump(get_loaded_extensions());
	}

	/**
	 * Prints a list of all HTTP request headers.
	 *
	 * @access public
	 * @static
	 */
	public static function headers()
	{
		// deal with fcgi installs on PHP 5.3
		if (version_compare(PHP_VERSION, '5.4.0') < 0 and  ! function_exists('apache_request_headers'))
		{
			$headers = array();
			foreach (\Input::server() as $name => $value)
			{
				if (strpos($name, 'HTTP_') === 0)
				{
					$name = str_replace(' ', '-', ucwords(strtolower(str_replace('_', ' ', substr($name, 5)))));
					$headers[$name] = $value;
				}
				elseif ($name == 'CONTENT_TYPE')
				{
					$headers['Content-Type'] = $value;
				}
				elseif ($name == 'CONTENT_LENGTH')
				{
					$headers['Content-Length'] = $value;
				}
			}
		}
		else
		{
			$headers = getAllHeaders();
		}

		return static::dump($headers);
	}

	/**
	 * Prints a list of the configuration settings read from <i>php.ini</i>
	 *
	 * @access public
	 * @static
	 */
	public static function phpini()
	{
		if ( ! is_readable(get_cfg_var('cfg_file_path')))
		{
			return false;
		}

		// render it
		return static::dump(parse_ini_file(get_cfg_var('cfg_file_path'), true));
	}

	/**
	 * Benchmark anything that is callable
	 *
	 * @access public
	 * @static
	 */
	public static function benchmark($callable, array $params = array())
	{
		// get the before-benchmark time
		if (function_exists('getrusage'))
		{
			$dat = getrusage();
			$utime_before = $dat['ru_utime.tv_sec'] + round($dat['ru_utime.tv_usec']/1000000, 4);
			$stime_before = $dat['ru_stime.tv_sec'] + round($dat['ru_stime.tv_usec']/1000000, 4);
		}
		else
		{
			list($usec, $sec) = explode(" ", microtime());
			$utime_before = ((float)$usec + (float)$sec);
			$stime_before = 0;
		}

		// call the function to be benchmarked
		$result = is_callable($callable) ? call_user_func_array($callable, $params) : null;

		// get the after-benchmark time
		if (function_exists('getrusage'))
		{
			$dat = getrusage();
			$utime_after = $dat['ru_utime.tv_sec'] + round($dat['ru_utime.tv_usec']/1000000, 4);
			$stime_after = $dat['ru_stime.tv_sec'] + round($dat['ru_stime.tv_usec']/1000000, 4);
		}
		else
		{
			list($usec, $sec) = explode(" ", microtime());
			$utime_after = ((float)$usec + (float)$sec);
			$stime_after = 0;
		}

		return array(
			'user' => sprintf('%1.6f', $utime_after - $utime_before),
			'system' => sprintf('%1.6f', $stime_after - $stime_before),
			'result' => $result
		);
	}

}

